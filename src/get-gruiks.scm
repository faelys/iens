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
  (chicken process signal)
  (chicken process-context)
  (chicken string)
  (chicken time)
  (chicken time posix)
  atom
  openssl ; must be above http-client
  http-client
  intarweb
  nanosleep
  rss
  sql-de-lite
  uri-common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command-Line Processing

(define arg-list (command-line-arguments))

(define db-name
  (if (>= (length arg-list) 1)
      (car arg-list)
      "iens.sqlite"))

(define total-period
  (if (>= (length arg-list) 2)
      (string->number (list-ref arg-list 1))
      #f))

;;;;;;;;;;;;;;;;;;;;;;;
;; Persistent Storage

(define db
  (open-database db-name))
(exec (sql/transient db "PRAGMA foreign_keys = ON;
                         PRAGMA journal_mode = WAL;
                         PRAGMA synchronous = NORMAL;
                         PRAGMA busy_timeout = 5000;"))
(set-busy-handler! db (busy-timeout 10000))

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
    (when (= 0 (exec (sql db "UPDATE gruik
                              SET title=?,
                                  notes=trim(notes||char(10)
                                             ||'Previously “'||title||'”',
                                             char(10)),
                                  mtime=CAST(strftime('%s', 'now') AS INT)
                              WHERE url=? AND section=?
                                AND COALESCE(comment_url,'')=?;")
                     title url source (if comm comm "")))
      (exec
        (sql db "INSERT INTO gruik(position, notes, ptime,
                                   section, url, title, comment_url,
                                   mark, ctime, mtime)
                 VALUES (-1, '', datetime(?1,'unixepoch')||'*',
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
            0 -1)))
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
        (list 1
              (feed-entries feed)
              (title-text (feed-title feed)))
        #f)))

(define (get-rss url last-modified etag)
  (let ((feed (get-source rss:read url last-modified etag)))
    (if feed
        (list 2
              (rss:feed-items feed)
              (rss:item-title (rss:feed-channel feed)))
        #f)))

(define (get-auto url)
  (let* ((data (get-source read-string url '() '()))
         (da   (condition-case (with-input-from-string data atom:read)
                               ((atom) #f)))
         (dr   (condition-case (with-input-from-string data rss:read)
                               ((rss) #f))))
    (exec (sql db "UPDATE source_rss SET format=? WHERE url=?;")
      (cond (da 1) (dr 2) (else -1))
      url)
    (cond
      (da (list 1
                (feed-entries da)
                (title-text (feed-title da))))
      (dr (list 2
                (rss:feed-items dr)
                (rss:item-title (rss:feed-channel dr))))
      (else #f))))

(define (process-source name url format last-modified etag)
  (condition-case
    (let ((data (case format ((0) (get-auto url))
                             ((1) (get-atom url last-modified etag))
                             ((2) (get-rss  url last-modified etag))
                             (else #f))))
      (when data
        (let ((args (list
                      (if (string=? name url)
                          (begin
                            (exec (sql db "UPDATE source_rss SET name=?
                                           WHERE name=? AND url=?;")
                                  (caddr data) name url)
                            (caddr data))
                          name)
                      (cadr data))))
          (case (car data)
            ((1) (apply process-atom args))
            ((2) (apply process-rss  args))
            (else (assert #f "Bad process index"))))))
    (exn (user-interrupt) (signal exn))
    (exn () (write-line (conc "Error while checking " name))
            (print-error-message exn))))

;;;;;;;;;;;;;;;
;; Actual Run

(define (add-period prev-deadline)
  (+ (max prev-deadline (current-seconds))
     (/ total-period
        (query fetch-value (sql db "SELECT count(*) FROM source_rss;")))))

(define usr1-queue (make-signal-handler signal/usr1))

(if total-period
    (let loop ((index (query fetch-value
                             (sql/transient db
                               "SELECT min(id) FROM source_rss;")))
               (deadline (add-period 0)))
      (let ((arg (query fetch-row
                        (sql db "SELECT
                                   COALESCE((SELECT min(id) FROM source_rss
                                                            WHERE id > ?1),
                                            (SELECT min(id) FROM source_rss)),
                                   name,url,format,last_modified,etag
                                 FROM source_rss WHERE id = ?1;")
                        index)))
        (apply process-source (cdr arg))
        (let ((rest (- deadline (current-seconds))))
          (when (positive? rest)
            (secosleep rest)))
        (unless (and (<= (car arg) index) (usr1-queue))
          (loop (car arg) (add-period deadline)))))
    (query
      (for-each-row* process-source)
      (sql db "SELECT name,url,format,last_modified,etag FROM source_rss;")))
