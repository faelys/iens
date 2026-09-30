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
  (chicken file)
  (chicken file posix)
  (chicken io)
  (chicken port)
  (chicken process signal)
  (chicken process-context)
  (chicken string)
  (chicken time)
  (chicken time posix)
  atom
  comparse
  openssl ; must be above http-client
  http-client
  intarweb
  nanosleep
  rss
  sql-de-lite
  srfi-19-time
  uri-common)

(define verbosity
  (let ((var (get-environment-variable "VERBOSE")))
    (if var
        (let ((n (string->number var))) (if n n 1))
        0)))
(define (write-log n . args)
  (when (>= verbosity n)
    (let ((ts (time->string (seconds->local-time) "%H:%M:%S ")))
      (write-line (apply conc (cons ts args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheduling primitives

(define min-sleep (seconds->time 0.125))
(define (sleep-until deadline)
  (let* ((dt  (time-max min-sleep (time-difference deadline (monotonic-time))))
         (sec (time->seconds dt)))
    (write-log 2 " Sleeping for " (exact->inexact sec) "s")
    (secosleep sec)))
(define (run-until deadline count thunk)
  (if deadline
    (let* ((now (monotonic-time))
           (my-period (/ (time->seconds (time-difference deadline now)) count))
           (my-deadline (add-duration now (seconds->time my-period))))
      (thunk)
      (sleep-until my-deadline))
    (thunk)))

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

(define gruik-inserted 0)
(define gruik-processed 0)

(define (reset-gruik-counters)
  (set! gruik-inserted 0)
  (set! gruik-processed 0))

(define (zero-gruik-counters)
  (unless (and (zero? gruik-inserted) (zero? gruik-processed))
    (write-log 0 "Unexpected processing of "
                 gruik-inserted "/" gruik-processed " gruiks")
    (reset-gruik-counters)))

(define (process-gruik source url title comm)
  (set! gruik-processed (add1 gruik-processed))
  (when (= 0 (exec (sql db "UPDATE gruik
                            SET lastseen=CAST(strftime('%s', 'now') as INT),
                                comment_url=?
                            WHERE section=? AND url=? AND title=?
                              AND (comment_url IS NULL OR comment_url=?1);")
                   (if comm comm '()) source url title)
             (query fetch-value
                    (sql db "SELECT count(id) FROM entry
                             WHERE source=? AND url=? AND title=?;")
                    source url title))
    (when (= 0 (exec (sql db "UPDATE gruik
                              SET title=?,
                                  comment_url=?,
                                  notes=trim(notes||char(10)
                                             ||'Previously “'||title||'”',
                                             char(10)),
                                  lastseen=CAST(strftime('%s', 'now') AS INT)
                              WHERE url=? AND section=?
                                AND (comment_url IS NULL OR comment_url=?2);")
                     title (if comm comm '()) url source))
      (set! gruik-inserted (add1 gruik-inserted))
      (exec
        (sql db "INSERT INTO gruik(position, notes, ptime,
                                   section, url, title, comment_url,
                                   mark, ctime, mtime, lastseen)
                 VALUES (-1, '', datetime(?1,'unixepoch')||'*',
                         ?2, ?3, ?4, ?5,
                         ?6, ?1, ?1, CAST(strftime('%s', 'now') as INT));")
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
            0 -1)))))

(define (process-atom deadline source items)
  (unless (null? items)
    (run-until deadline (length items)
      (lambda ()
        (process-gruik source
                       (link-uri (car (entry-links (car items))))
                       (title-text (entry-title (car items)))
                       #f)))
    (process-atom deadline source (cdr items))))

(define (process-rss deadline source items)
  (unless (null? items)
    (let* ((item  (car items))
           (attr  (rss:item-attributes item))
           (link  (rss:item-link item))
           (title (rss:item-title item))
           (comm  (alist-ref 'comments attr)))
      (run-until deadline (length items)
        (lambda () (process-gruik source link (if title title link) comm)))
      (process-rss deadline source (cdr items)))))

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

(define (process-source deadline name url format last-modified etag)
  (zero-gruik-counters)
  (write-log 1 "Processing source " name)
  (condition-case
    (let ((data (case format ((0) (get-auto url))
                             ((1) (get-atom url last-modified etag))
                             ((2) (get-rss  url last-modified etag))
                             (else #f))))
      (if data
        (let ((args (list
                      (if (and deadline (not (null? deadline))) deadline #f)
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
            (else (assert #f "Bad process index")))
          (write-log 1 "Inserted " gruik-inserted "/" gruik-processed " gruiks")
          (reset-gruik-counters))
        (exec (sql db "UPDATE gruik
                       SET lastseen=CAST(strftime('%s', 'now') as INT)
                       WHERE section=?;")
              name)))
    (exn (client-error)
      (write-line (conc "Error while checking " name))
      (write-line (conc "  Headers: " (response-headers ((condition-property-accessor 'client-error 'response) exn))))
      (print-error-message exn))
    (exn (user-interrupt) (signal exn))
    (exn () (write-line (conc "Error while checking " name))
            (print-error-message exn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gruik build from IRC log

(define irc-digit      (in #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(define irc-hex        (in #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
                           #\8 #\9 #\a #\b #\c #\d #\e #\f))
(define (irc-digits n) (repeated irc-digit n))
(define irc-date
  (as-string
    (sequence (irc-digits 4) (is #\.)
              (irc-digits 2) (is #\.)
              (irc-digits 2) (is #\ )
              (irc-digits 2) (is #\:)
              (irc-digits 2) (is #\:)
              (irc-digits 2))))
(define irc-nick
  (as-string
    (enclosed-by (is #\<)
                 (repeated item until: (is #\>))
                 (is #\>))))
(define irc-source
  (as-string
    (enclosed-by (char-seq " [")
                 (repeated item until: (is #\]))
                 (char-seq "] "))))
(define irc-url
  (as-string
    (enclosed-by (char-seq " ")
                 (sequence (char-seq "http")
                           (repeated item until: (is #\space)))
                 (char-seq " "))))
(define irc-hash
  (as-string
    (enclosed-by (char-seq "#")
                 (repeated irc-hex 8)
                 end-of-input)))
(define irc-suffix (sequence irc-url irc-hash))
(define irc-line
  (sequence irc-date
            irc-nick
            irc-source
            (as-string (repeated item until: irc-suffix))
            irc-url
            irc-hash))

(define (read-line-pos fd)
  (let loop ((acc ""))
    (let ((c (file-read fd 1)))
      (if (and (= 1 (cadr c))
               (not (string=? (car c) "\n")))
          (loop (string-append acc (car c)))
          (list acc (file-position fd))))))

(define (line->notes line max-width)
  (let loop ((rest (string-split line " " #t))
             (lines  '())
             (words  ""))
    (cond
      ((null? rest)
        (reverse-string-append (cons words lines)))
      ((<= (+ (string-length words) 1 (string-length (car rest))) max-width)
        (loop (cdr rest)
              lines
              (string-append words
                             (if (string=? words "") "" " ")
                             (car rest))))
      (else
        (loop (cdr rest)
              (cons (string-append words "\n") lines)
              (car rest))))))

(define (insert-line line offset)
  (set! gruik-processed (add1 gruik-processed))
  (secosleep (time->seconds (min-sleep)))
  (and-let* ((parsed  (parse irc-line line))
             (now     (current-seconds))
             (section (list-ref parsed 2))
             (title   (list-ref parsed 3))
             (url     (list-ref parsed 4))
             (_ (= 0 (exec (sql db
                             "UPDATE gruik
                              SET mtime=CAST(strftime('%s', 'now') as INT),
                                  notes=(CASE WHEN title=?3
                                         THEN notes
                                         ELSE trim(notes||char(10)
                                                   ||'Also “'||?3||'”',
                                                   char(10))
                                         END)
                              WHERE section=?1 AND url=?2;")
                           section url title)
                     (query fetch-value
                            (sql db "SELECT COUNT(id) FROM entry
                                     WHERE source=? AND url=? AND title=?;")
                            section url title))))
    (set! gruik-inserted (add1 gruik-inserted))
    (exec
      (sql db
        "INSERT INTO gruik(position, notes, ptime,
                           section, title, url, mark, ctime, mtime)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);")
      offset
      (line->notes line 79)
      (car parsed)
      section
      title
      url
      (+ (query fetch-value
                (sql db "SELECT -2*COUNT(*) FROM gruik WHERE url=?;")
                url)
         (query fetch-value
                (sql db "SELECT -2*COUNT(*) FROM entry WHERE url=?;")
                url))
      now
      now)))

(define (import-gruiks)
  (let ((src-path (get-config "gruik-source")))
    (when src-path
      (let* ((fd (file-open src-path open/rdonly))
             (so (get-config/default "gruik-seen" 0))
             (_  (set-file-position! fd so seek/set)))
        (zero-gruik-counters)
        (write-log 1 "Importing gruiks from " so)
        (let loop ((offset so))
          (let ((rp (read-line-pos fd)))
            (if (= (cadr rp) offset)
              (begin
                (write-log 1 "Imported " gruik-inserted "/" gruik-processed
                             " gruiks until " offset)
                (reset-gruik-counters)
                (exec
                  (sql db "INSERT OR REPLACE INTO config VALUES (?,?);")
                  "gruik-seen"
                  offset))
              (begin
                (apply insert-line rp)
                (loop (cadr rp))))))))))

;;;;;;;;;;;;;;;
;; Actual Run

(define (source-deadline)
  (add-duration
    (monotonic-time)
    (seconds->time
      (/ total-period
         (query fetch-value (sql db "SELECT count(*) FROM source_rss;"))))))

(define usr1-queue (make-signal-handler signal/usr1))

(import-gruiks)

(if total-period
    (let loop ((index (query fetch-value
                             (sql/transient db
                               "SELECT min(id) FROM source_rss;"))))
      (let ((deadline (source-deadline))
            (arg (query fetch-row
                        (sql db "SELECT
                                   COALESCE((SELECT min(id) FROM source_rss
                                                            WHERE id > ?1),
                                            (SELECT min(id) FROM source_rss)),
                                   name,url,format,last_modified,etag
                                 FROM source_rss WHERE id = ?1;")
                        index)))
        (apply process-source (cons deadline (cdr arg)))
        (sleep-until deadline)
        (when (<= (car arg) index)
          (import-gruiks))
        (unless (and (<= (car arg) index) (usr1-queue))
          (loop (car arg)))))
    (query
      (for-each-row* process-source)
      (sql db "SELECT NULL,name,url,format,last_modified,etag
               FROM source_rss;")))
