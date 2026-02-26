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
  (chicken file posix)
  (chicken io)
  (chicken process-context)
  (chicken string)
  (chicken time)
  comparse
  sql-de-lite
  sxml-serializer)

(define css-style "
.bad-post { background: #fcc; }
.marked-post { backgound: #ccf; }
")

(define content-length
  (let ((ct (get-environment-variable "CONTENT_LENGTH")))
    (if ct (string->number ct) 0)))
(define input-text (read-string content-length))
(define input-list
  (map
    (lambda (s)
      (let ((index (substring-index "=" s)))
        (if index
            (list (substring s 0 index) (substring s (+ 1 index)))
            s)))
    (string-split input-text "&")))
(define (input-var name)
  (let loop ((rest input-list))
    (cond ((null? rest) #f)
          ((string=? (caar rest) name) (cadar rest))
          (else (loop (cdr rest))))))

(define start-html
  "Content-Type: text/html\r\n\r\n<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">")

(define (html-output form)
  (write-string start-html)
  (serialize-sxml form
    method: 'html
    output: (current-output-port)))

(define (htmx-output form)
  (write-string "Content-Type: text/html\r\n\r\n")
  (serialize-sxml form
    method: 'html
    output: (current-output-port)))

(define (debug-output)
  (html-output
    `(html
      (head (title "Variable dump"))
      (body (h1 "Variable dump")
        (p "Current directory: " ,(current-directory))
        (table
          ,@(map
              (lambda (pair)
                `(tr (td ,(car pair)) (td ,(cdr pair))))
              (get-environment-variables)))
        (h2 "Inputs")
        (pre (code ,input-text))
        (table
          ,@(map
              (lambda (l) (cons 'tr (map (lambda (c) (list 'td c)) l)))
              input-list))))))

(define (die msg)
  (write-string "Status: 500\r\n")
  (when msg
    (write-string "Content-Type: text/plain\r\n\r\n")
    (write-string msg))
  (exit 1))
(define (bad-input msg)
  (write-string "Status: 400\r\n")
  (when msg
    (write-string "Content-Type: text/plain\r\n\r\n")
    (write-string msg))
  (exit 0))

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



(define root (get-environment-variable "DOCUMENT_ROOT"))
(when (not root)
  (die "Missing $DOCUMENT_ROOT"))
(define db-name (string-append root "/./iens.sqlite"))

(define db (open-database db-name))
(exec (sql/transient db "PRAGMA foreign_keys = ON;"))
(define (db-version)
  (query fetch-value (sql db "PRAGMA user_version;")))

(when (= 2 (db-version))
  (for-each
    (lambda (s) (exec (sql/transient db s)))
    (list "CREATE TABLE gruik
            (id INTEGER PRIMARY KEY,
             position INTEGER NOT NULL,
             notes TEXT NOT NULL,
             description TEXT,
             ptime INTEGER NOT NULL,
             section TEXT NOT NULL,
             title TEXT NOT NULL,
             url TEXT NOT NULL,
             mark INTEGER NOT NULL DEFAULT FALSE,
             ctime INTEGER NOT NULL,
             mtime INTEGER NOT NULL);"
          "CREATE UNIQUE INDEX i_gruik ON gruik(position);"
          "CREATE INDEX i_gruik_time ON gruik(ptime);"
          "CREATE TABLE gruik_tags
            (gruik_id REFERENCES gruik(id) ON UPDATE CASCADE ON DELETE CASCADE,
             tag_id REFERENCES tag(id) ON UPDATE CASCADE ON DELETE CASCADE);"
          "CREATE UNIQUE INDEX i_gruik_rel ON gruik_tags(gruik_id,tag_id);"
          "CREATE INDEX i_gruik_tags ON gruik_tags(tag_id,gruik_id);"
          "INSERT INTO config(key, val) VALUES ('gruik-source', '/home/nat/irclogs/libera/#gcufeed.log');"
          "INSERT INTO config(key, val) VALUES ('gruik-seen', 38678556);"
          "INSERT INTO config(key, val) VALUES ('gruik-host', 'https://users.instinctive.eu');"
          "INSERT INTO config(key, val) VALUES ('gruik-prefix', '/iens');"
          "PRAGMA user_version = 3;")))

(unless (= 3 (db-version))
  (die "Unexpectad database version"))


(define (get-config key)
  (query fetch-value (sql db "SELECT val FROM config WHERE key = ?;") key))

(define (get-config/default key default-value)
  (let ((result (get-config key)))
    (if result
        result
        default-value)))

(define (insert-line line offset)
  (let ((parsed (parse irc-line line))
        (now    (current-seconds)))
    (when parsed
      (exec
        (sql db
          "INSERT INTO gruik(position, notes, ptime, section, title, url, ctime, mtime) VALUES (?, ?, ?, ?, ?, ?, ?, ?);")
        offset
        line
        (car parsed)
        (list-ref parsed 2)
        (list-ref parsed 3)
        (list-ref parsed 4)
        now
        now))))

(define (catch-up)
  (let ((src-path (get-config "gruik-source")))
    (when (not src-path) (die "No source configured"))
    (let* ((fd (file-open src-path open/rdonly))
           (so (get-config/default "gruik-seen" 0))
           (_  (set-file-position! fd so seek/set)))
      (let loop ((offset so))
        (let ((rp (read-line-pos fd)))
          (if (= (cadr rp) offset)
            (exec
              (sql/transient db "INSERT OR REPLACE INTO config VALUES (?,?);")
              "gruik-seen"
              offset)
            (begin
              (apply insert-line rp)
              (loop (cadr rp)))))))))

(define (redirect location)
  (write-string "Status: 302\r\nLocation: ")
  (write-string (get-config/default "gruik-host" ""))
  (write-string (get-config/default "gruik-prefix" ""))
  (write-string location)
  (write-string "\r\n\r\n"))

(define (post-p-fragment ptime section title url)
  `(p
    (span (@ (class "ptime")) ,ptime)
    (span (@ (class "section")) ,section)
    (span (@ (class "title")) ,title)
    (a (@ (href ,url)) ,url)))

(define (bad-post-fragment id ptime section title url)
  `(form (@ (method "POST") (action "do-undelete") (class "bad-post"))
    ,(post-p-fragment ptime section title url)
    (input (@ (type "hidden") (name "id") (value ,id)))
    (input (@ (type "submit") (name "submit") (value "Restore")))))

(define (marked-post-fragment id ptime section title url)
  `(form (@ (method "POST") (action "do-marked") (class "marked-post"))
    ,(post-p-fragment ptime section title url)
    (input (@ (type "hidden") (name "id") (value ,id)))
    (input (@ (type "submit") (name "submit") (value "Unmark")))
    (input (@ (type "submit") (name "submit") (value "Edit")))))

(define (unmarked-post-fragment id ptime section title url)
  `(form (@ (method "POST") (action "do-unmarked") (class "unmarked-post"))
    ,(post-p-fragment ptime section title url)
    (input (@ (type "hidden") (name "id") (value ,id)))
    (input (@ (type "submit") (name "submit") (value "Mark")))
    (input (@ (type "submit") (name "submit") (value "Delete")))))

(define (post-fragment id mark ptime section title url)
  (case mark
    ((0)  (unmarked-post-fragment id ptime section title url))
    ((1)  (marked-post-fragment   id ptime section title url))
    (else (bad-post-fragment      id ptime section title url))))

(define (gruik-list-view title q)
  (html-output
    `(html
      (head
        (title ,title)
        (script (@ (src "https://cdn.jsdelivr.net/npm/htmx.org@2.0.8/dist/htmx.min.js")) ""))
        (style ,css-style)
      (body (h1 ,title)
        ,@(query
           (map-rows* post-fragment)
           (sql db q))))))

(define (deleted-view)
  (catch-up)
  (gruik-list-view
    "Deleted gruiks"
    "SELECT id,mark,ptime,section,title,url FROM gruik WHERE mark < 0 ORDER BY mtime;"))

(define (main-view)
  (catch-up)
  (gruik-list-view
    "Latest gruiks"
    "SELECT id,mark,ptime,section,title,url FROM gruik WHERE mark >= 0;"))

(define (do-set-mark id old-v new-v)
  (exec (sql db "UPDATE gruik SET mark = ? WHERE mark = ? AND id = ?;")
        new-v old-v id))

(define (do-marked)
  (let ((id     (input-var "id"))
        (submit (input-var "submit")))
    (cond
      ((not id)                   (bad-input "missing id"))
      ((not submit)               (bad-input "missing submit"))
      ((string=? submit "Edit")   (redirect (conc "/gruik/" id)))
      ((string=? submit "Unmark") (do-set-mark id 0 -1) (redirect "/"))
      (else                       (bad-input "bad value for submit")))))

(define (do-undelete)
  (let ((id     (input-var "id"))
        (submit (input-var "submit")))
    (cond
      ((not id)                    (bad-input "missing id"))
      ((not submit)                (bad-input "missing submit"))
      ((string=? submit "Restore") (do-set-mark id -1 0) (redirect "/"))
      (else                        (bad-input "bad value for submit")))))

(define (do-unmarked)
  (let ((id     (input-var "id"))
        (submit (input-var "submit")))
    (cond
      ((not id)                   (bad-input "missing id"))
      ((not submit)               (bad-input "missing submit"))
      ((string=? submit "Mark")   (do-set-mark id 0  1) (redirect "/"))
      ((string=? submit "Delete") (do-set-mark id 0 -1) (redirect "/"))
      (else                       (bad-input "bad value for submit")))))


(defrne route-do-marked
  (preceded-by (char-seq "do-marked")
               (result do-marked)))
(define route-do-undelete
  (preceded-by (char-seq "do-undelete")
               (result do-undelete)))
(define route-do-unmarked
  (preceded-by (char-seq "do-unmarked")
               (result do-unmarked)))
(define route-do-unmarked
  (preceded-by (char-seq "do-unmarked")
               (result do-unmarked)))
(define route-deleted
  (preceded-by (char-seq "deleted")
               (result deleted-view)))
(define route-main (result main-view))
(define route-ok
  (preceded-by (char-seq "ok")
               (result (lambda ()
                 (write-string "Content-Type: text/plain\r\n\r\nOK\n")))))

(define router
  (preceded-by (char-seq (get-config/default "gruik-prefix" ""))
               (is #\/)
               (apply any-of
                 (map (lambda (p) (followed-by p end-of-input))
                   (list route-do-marked
                         route-do-undelete
                         route-do-unmarked
                         route-deleted
                         route-main
                         route-ok)))))

(let* ((uri (get-environment-variable "REQUEST_URI"))
       (_   (if uri uri (die "Missing $REQUEST_URI")))
       (fn  (parse router uri)))
  (if fn
    (fn)
    (debug-output)))
