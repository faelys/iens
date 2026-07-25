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
  (chicken io)
  (chicken process-context)
  (chicken string)
  (chicken time posix)
  openssl ; must be above http-client
  http-client
  rss
  sql-de-lite)

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

(assert (= 7 (db-version)))

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
               VALUES (-?1, '', datetime(?1,'unixepoch'),
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

(define (process-rss source items)
  (unless (null? items)
    (let* ((item  (car items))
           (attr  (rss:item-attributes item))
           (link  (rss:item-link item))
           (title (rss:item-title item))
           (comm  (alist-ref 'comments attr)))
      (process-gruik source link (if title title link) comm)
      (process-rss source (cdr items)))))

(define (process-source name url)
  (let* ((rss (with-input-from-request url #f rss:read))
         (source (if (string=? name url)
                     (begin
                       (exec (sql db "UPDATE source_rss SET name=?
                                      WHERE name=? AND url=?;")
                             (rss:item-title (rss:feed-channel rss))
                             name url)
                       (rss:item-title (rss:feed-channel rss)))
                     name)))
    (assert source)
    (process-rss source (rss:feed-items rss))))

;;;;;;;;;;;;;;;
;; Actual Run

(query
  (for-each-row* process-source)
  (sql db "SELECT name,url FROM source_rss;"))
