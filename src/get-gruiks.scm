; Copyright (c) 2026, Natacha Porté
;
; Permission to use, copy, modify, and distribute this software for any
; purpose with or without fee is hereby granted, provided that the above
; copyright notice and this permission notice appear in all copies.
;
; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(import
  (chicken condition)
  (chicken io)
  (chicken port)
  (chicken process-context)
  (chicken string)
  (chicken time posix)
  atom
  openssl ; must be above http-client
  http-client
  intarweb
  rss
  sql-de-lite
  uri-common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command-Line Processing

(define db-name
  (let ((arg-list (command-line-arguments)))
    (if (>= (length arg-list) 1)
      (car arg-list)
      "iens.sqlite")))

;;;;;;;;;;;;;;;;;;;;;;;
;; Persistent Storage

(define db
  (open-database db-name))
(exec (sql/transient db "PRAGMA foreign_keys = ON;
                         PRAGMA busy_timeout = 5000;"))
; PRAGMA journal_mode = WAL;
; PRAGMA synchronous = NORMAL;

(include "common.scm")

(assert (= 8 (db-version)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gruik build from sources

(define (process-gruik source url title comm)
  (if (= 0 (query fetch-value
                  (sql db "SELECT count(id) FROM gruik
                           WHERE section=? AND url=? AND title=?;")
                  source url title)
           (query fetch-value
                  (sql db "SELECT count(id) FROM entry
                           WHERE source=? AND url=? AND title=?;")
                  source url title))
    (exec
      (sql db "INSERT INTO gruik(position, notes, ptime,
                                 section, url, title, comment_url,
                                 mark, ctime, mtime)
               VALUES (-1, '', datetime(?1,'unixepoch'),
                       ?2, ?3, ?4, ?5,
                       ?6, ?1, ?1);")
      (query fetch-value
             (sql db "SELECT MAX(CAST(strftime('%s', 'now') as INT),
                                 (SELECT max(mtime) FROM gruik) + 1);"))
      source url title (if comm comm '())
      (if (= 0 (query fetch-value
                      (sql db "SELECT count(id) FROM gruik WHERE url=?;")
                      url)
               (query fetch-value
                      (sql db "SELECT count(id) FROM entry WHERE url=?;")
                      url))
          0 -1))
    (exec (sql db "UPDATE gruik
                   SET mtime=MAX(CAST(strftime('%s', 'now') as INT),
                                 (SELECT max(mtime) FROM gruik) + 1)
                   WHERE section=? AND url=? AND title=? AND mark<0;")
          source url title)))

(define (process-atom source items)
  (unless (null? items)
    (process-gruik source
                   (link-uri (car (entry-links (car items))))
                   (title-text (entry-title (car items)))
                   #f)
    (process-atom source (cdr items))))

(define (process-rss source items)
  (unless (null? items)
    (let* ((item  (car items))
           (attr  (rss:item-attributes item))
           (link  (rss:item-link item))
           (title (rss:item-title item))
           (comm  (alist-ref 'comments attr)))
      (process-gruik source link (if title title link) comm)
      (process-rss source (cdr items)))))

(define (absorb-304 req parse)
  (condition-case
    (with-input-from-request req #f parse)
    ((exn unexpected-server-response) (values #f #f #f))))

(define (get-source parse url last-modified etag)
  (let* ((hlm (if (null? last-modified) '()
                  `((if-modified-since
                      #(,(seconds->local-time last-modified) ())))))
         (het (cond ((null? etag) '())
                    ((string=? etag "") '())
                    ((eqv? (string-ref etag 0) #\S)
                      `((if-none-match (strong . ,(substring etag 1)))))
                    ((eqv? (string-ref etag 0) #\W)
                      `((if-none-match (weak . ,(substring etag 1)))))
                    (else '())))
         (req (make-request
                uri: (uri-reference url)
                headers: (headers `(,@hlm ,@het)))))
    (let-values (((result _ resp) (absorb-304 req parse)))
      (when resp
        (let* ((hdr (response-headers resp))
               (lm  (header-value 'last-modified hdr))
               (et  (header-value 'etag hdr)))
          (when (or (not (null? last-modified)) (not (null? etag)) lm et)
            (exec (sql db "UPDATE source_rss SET last_modified=?, etag=?
                           WHERE url=?;")
                  (if lm (local-time->seconds lm) '())
                  (if et (string-append
                           (cond ((eq? (car et) 'weak) "W")
                                 ((eq? (car et) 'strong) "S")
                                 (else "*"))
                           (cdr et))
                      '())
                  url))))
      result)))

(define (atom:read) (read-atom-feed (current-input-port)))
(define (get-atom url last-modified etag)
  (let ((feed (get-source atom:read url last-modified etag)))
    (if feed
        (list process-atom
              (feed-entries feed)
              (title-text (feed-title feed)))
        #f)))

(define (get-rss url last-modified etag)
  (let ((feed (get-source rss:read url last-modified etag)))
    (if feed
        (list process-rss
              (rss:feed-items feed)
              (rss:item-title (rss:feed-channel feed)))
        #f)))

(define (process-source name url format last-modified etag)
  (condition-case
    (let ((data (case format ((1) (get-atom url last-modified etag))
                             ((2) (get-rss  url last-modified etag))
                             (else #f))))
      (when data
        (let ((source (if (string=? name url)
                          (begin
                            (exec (sql db "UPDATE source_rss SET name=?
                                           WHERE name=? AND url=?;")
                                  (caddr data) name url)
                            (caddr data))
                          name)))
          ((car data) source (cadr data)))))
    (exn () (write-line (conc "Error while checking " name))
            (print-error-message exn))))


;;;;;;;;;;;;;;;
;; Actual Run

(query
  (for-each-row* process-source)
  (sql db "SELECT name,url,format,last_modified,etag FROM source_rss;"))
