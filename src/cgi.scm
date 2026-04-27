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
  openssl ; must be above http-client
  http-client
  rss
  sql-de-lite
  sxml-serializer)

(define css-style #<<END-OF-CSS
h1 { text-align: center; }
pre { overflow: scroll; }
.form-body { overflow: scroll; }
.bad-post { background: #fcc; }
.marked-post { background: #ccf; }
.locked-post { background: #cff; }
.protected-post { background: #cfc; }
form {
  margin: 1rex 0;
  display: grid;
  gap: 0.5rex;
  transition: all 0.5s ease-in;
}
.lsub { width: 4.5rem; height: 3rem; }
.rsub { width: 4.5rem; height: 3rem; }
textarea { display: block; max-width: 100%; }
.tag-list { column-width: 10rem; column-gap: 1rem; }
.tag-list label { display: block; }
span.ptime { font-size: 80%; }
span.section { font-size: 80%; }
a.section { font-size: 80%; }
span.title { font-weight: bold; display: block; }
@media (min-width: 60rem) {
  form {
    grid-template-columns: 5rem 1fr 5rem;
    align-items: center;
  }

  .form-body { grid-column: 2; }
  .lsub { grid-column: 1; justify-self: start; }
  .rsub { grid-column: 3; justify-self: end; }
}
@media (max-width: 59.9rem) {
  form {
    grid-template-columns: 1fr 1fr;
    grid-template-areas: "c c" "l r";
  }

  .form-body { grid-area: c; }
  .lsub { grid-area: l; justify-self: start; }
  .rsub { grid-area: r; justify-self: end; }
  #load-new input, #load-new svg { grid-area: c; }
}

#load-new { text-align: center; grid-template-columns: auto; }
#load-new input { width: 4.5rem; height: 3rem; margin: auto; }
#load-new svg   { width: 4.5rem; height: 3rem; margin: auto; fill: #494949; }
#load-new svg { display: none; }
#load-new.htmx-request svg { display: block; }
.htmx-request input { display: none; }

body { background: #F0ECE0; color: #000000; }
form { background: #FFFFFF; }
a:link { color: #007FBF; }
a:visited { color: #003F7F; }
a:hover { background: #007FBF; color: #F0E8E0; }

@media (prefers-color-scheme: dark) {
  body { background: #103c48; color: #adbcbc; }
  form { background: #184956; color: #cad8d9; }
  a:link { color: #4695f7; }
  a:visited { color: #af88eb; }
  a:hover { background: #4695f7; color: #103c48; }
  .bad-post { background: #783946; }
  .marked-post { background: #1849a6; }
  .locked-post { background: #189999; }
  .protected-post { background: #189956; }
  #load-new svg { fill: #cad8d9; }
}
END-OF-CSS
)

(define content-length
  (let ((ct (get-environment-variable "CONTENT_LENGTH")))
    (if ct (string->number ct) 0)))
(define input-text (read-string content-length))
(define input-list
  (let* ((hdigit* (any-of (preceded-by (is #\0) (result  0))
                          (preceded-by (is #\1) (result  1))
                          (preceded-by (is #\2) (result  2))
                          (preceded-by (is #\3) (result  3))
                          (preceded-by (is #\4) (result  4))
                          (preceded-by (is #\5) (result  5))
                          (preceded-by (is #\6) (result  6))
                          (preceded-by (is #\7) (result  7))
                          (preceded-by (is #\8) (result  8))
                          (preceded-by (is #\9) (result  9))
                          (preceded-by (is #\a) (result 10))
                          (preceded-by (is #\A) (result 10))
                          (preceded-by (is #\b) (result 11))
                          (preceded-by (is #\B) (result 11))
                          (preceded-by (is #\c) (result 12))
                          (preceded-by (is #\C) (result 12))
                          (preceded-by (is #\d) (result 13))
                          (preceded-by (is #\D) (result 13))
                          (preceded-by (is #\e) (result 14))
                          (preceded-by (is #\E) (result 14))
                          (preceded-by (is #\f) (result 15))
                          (preceded-by (is #\F) (result 15))))
         (pct*    (sequence* ((_ (is #\%))
                              (h hdigit*)
                              (l hdigit*))
                    (result (integer->char (+ (* 16 h) l)))))
         (value*  (as-string (repeated (any-of pct* item) until: (is #\&))))
         (name*   (as-string (repeated item until: (is #\=))))
         (pair*   (sequence* ((n name*)
                              (_ (is #\=))
                              (v value*)
                              (_ (is #\&)))
                    (result (list n (string-translate v "\r")))))
         (parser  (zero-or-more pair*)))
    (parse parser (string-append input-text "&"))))
(define (input-var name)
  (let loop ((rest input-list))
    (cond ((null? rest) #f)
          ((string=? (caar rest) name) (cadar rest))
          (else (loop (cdr rest))))))
(define (required-input-var name)
  (let ((val (input-var name)))
    (if val val (bad-input (conc "missing " name)))))

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
(define db-name (get-environment-variable "IENS_DB"))
(when (not db-name)
    (die "Missing $IENS_DB"))

(define db (open-database db-name))
(exec (sql/transient db "PRAGMA foreign_keys = ON;"))

(include "common.scm")

(unless (= 5 (db-version))
  (die "Unexpectad database version"))


(define (get-config key)
  (query fetch-value (sql db "SELECT val FROM config WHERE key = ?;") key))

(define (get-config/default key default-value)
  (let ((result (get-config key)))
    (if result
        result
        default-value)))

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
  (let ((parsed (parse irc-line line))
        (now    (current-seconds)))
    (when parsed
      (let ((url (list-ref parsed 4)))
        (exec
          (sql db
            "INSERT INTO gruik(position, notes, ptime, section, title, url, mark, ctime, mtime) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);")
          offset
          (line->notes line 79)
          (car parsed)
          (list-ref parsed 2)
          (list-ref parsed 3)
          url
          (+ (query fetch-value
                    (sql db "SELECT -2*COUNT(*) FROM gruik WHERE url=?;")
                    url)
             (query fetch-value
                    (sql db "SELECT -2*COUNT(*) FROM entry WHERE url=?;")
                    url))
          now
          now)))))

(define (catch-up)
  (let* ((span (get-config "gruik-clean")))
    (when (number? span)
      (exec
        (sql db "DELETE FROM gruik WHERE mark<0 AND mtime<?;")
        (- (current-seconds) span))))
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

(define (comment-link section url)
  (let* ((rss-url  (query fetch-value
                          (sql db "SELECT url FROM source_rss WHERE name=?;")
                          section)))
    (if rss-url
      (let ((rss (with-input-from-request rss-url #f rss:read)))
        (let loop ((items (rss:feed-items rss)))
          (cond
            ((null? items) #f)
            ((string=? url (rss:item-link (car items)))
              (alist-ref 'comments (rss:item-attributes (car items))))
            (else (loop (cdr items))))))
      #f)))

(define (auto-descr id)
  (let ((row (query fetch-row
                    (sql db "SELECT section,url FROM gruik
                             WHERE id=? AND COALESCE(description,'')='';")
                    id)))
    (unless (null? row)
      (let ((section (car row))
            (url     (cadr row))
            (comm    (apply comment-link row)))
        (if comm
          (exec
            (sql db "UPDATE gruik
                     SET description=?,
                         notes=trim(notes||char(10)||?,char(10)),
                         comment_url=?
                     WHERE id=? AND COALESCE(description,'')='';")
            (conc " + [](" url ")\n(via [" section "](" comm ") sur #gcufeed)")
            comm
            comm
            id)
          (exec
            (sql db "UPDATE gruik SET description=?
                     WHERE id=? AND COALESCE(description,'')='';")
            (conc " + [](" url ")\n(via " section " sur #gcufeed)")
            id))))))

(define (spinner-bar x y height beg)
  `(rect (@ (x ,x) (y ,y) (width 15) (height ,height) (rx 6))
    (animate (@ (attributeName height) (begin ,beg) (dur "1s")
                (values "120;110;100;90;80;70;60;50;40;140;120")
                (calcMode linear) (repeatCount indefinite)))
    (animate (@ (attributeName y) (begin ,beg) (dur "1s")
                (values "10;15;20;25;30;35;40;45;50;0;10")
                (calcMode linear) (repeatCount indefinite)))))
(define (spinner)
  `(svg (@ (width 16) (height 16) (class spinner)
           (viewBox "0 0 135 140") (xmlns "http://www.w3.org/2000/svg"))
    ,(spinner-bar   0 10 120 "0.5s")
    ,(spinner-bar  30 10 120 "0.25s")
    ,(spinner-bar  60  0 140 "0s")
    ,(spinner-bar  90 10 120 "0.25s")
    ,(spinner-bar 120 10 120 "0.5s")))

(define (post-p-fragment id ptime section title url comm-url)
  `(p
    (span (@ (class "ptime") (title ,id)) ,ptime)
    ,(if (null? comm-url)
         `(span (@ (class "section")) ,section)
         `(a (@ (href ,comm-url) (class "section")) ,section))
    (span (@ (class "title")) ,title)
    (a (@ (href ,url)) ,url)))

(define (edit-post-fragment id ptime section title url comm-url mark notes description)
  `(form (@ (method "POST") (action "do-edit")
            (id ,(conc "post-" id)) (class "edit-post")
            (hx-swap "outerHTML")  (hx-post "xdo-edit"))
    (input (@ (type "submit") (name "submit") (class lsub) (value "Edit")))
    (div (@ (class "form-body"))
      ,(post-p-fragment id ptime section title url comm-url)
      (p ,(conc "Mark: " mark)
        (label (input (@ (type radio) (name mark) (value 0))) "Unmark")
        (label (input (@ (type radio) (name mark) (value 1) (checked))) "Keep")
        (label (input (@ (type radio) (name mark) (value 2))) "Lock")
        (label (input (@ (type radio) (name mark) (value 3))) "Protect"))
      (pre (code ,notes))
      ,@(if (null? comm-url)
            `((p (label (input (@ (type checkbox) (name retry-comm) (value y)))
                               "Retry fetching comment URL")))
            '())
      (p (label "Append to notes:"
        (textarea (@ (name "notes") (cols 80) (rows 5)) "")))
      (p (label "Description:"
        (textarea (@ (name "description") (cols 80) (rows 12)) ,description)))
      (fieldset (legend "Tags")
        (details (@ (class tag-list)) (summary "Tags")
          ,@(query
              (map-rows*
                (lambda (tid name checked)
                  `(label
                    (input (@ (type checkbox) (name tags) (value ,tid)
                      ,@(if (= 0 checked) '() '((checked)))))
                    ,name)))
              (sql db "SELECT id,name,EXISTS (SELECT * FROM gruik_tags WHERE gruik_id=? AND tag_id = tag.id) FROM tag;")
              id))))
    (input (@ (type "hidden") (name "id") (value ,id)))
    (input (@ (type "submit") (name "submit") (class rsub) (value "Cancel")))))

(define (edit-post-fragment* id)
  (query
    (map-rows* edit-post-fragment)
    (sql db "SELECT id,ptime,section,title,url,comment_url,mark,notes,description FROM gruik WHERE mark=1 AND id=?;")
    id))

(define (db-edit)
  (let ((id         (string->number (required-input-var "id")))
        (retry-comm (input-var "retry-comm")))
    (when (string=? "Edit" (required-input-var "submit"))
      (exec
        (sql/transient db
          "UPDATE gruik SET mtime=?,notes=trim(notes||char(10)||?,char(10)),description=?,mark=? WHERE mark=1 AND id=?;")
        (current-seconds)
        (required-input-var "notes")
        (if retry-comm "" (required-input-var "description"))
        (string->number (required-input-var "mark"))
        id)
      (when retry-comm (auto-descr id))
      (let* ((n-tags (query fetch-value (sql db "SELECT MAX(id) FROM tag")))
             (tags   (make-vector (+ 1 n-tags) 0)))
        (let loop ((var input-list))
          (unless (null? var)
            (when (string=? (caar var) "tags")
              (vector-set! tags (string->number (cadar var)) 1))
            (loop (cdr var))))
        (query
          (for-each-row*
            (lambda (tid) (vector-set! tags tid (- (vector-ref tags tid) 1))))
          (sql db "SELECT tag_id FROM gruik_tags WHERE gruik_id=?;")
          id)
        (let loop ((tid n-tags))
          (unless (= 0 tid)
            (case (vector-ref tags tid)
              ((1)
                (exec
                  (sql db "INSERT INTO gruik_tags(gruik_id,tag_id) VALUES (?,?);")
                  id tid))
              ((-1)
                (exec
                  (sql db "DELETE FROM gruik_tags WHERE gruik_id=? AND tag_id=?;")
                  id tid)))
            (loop (- tid 1))))))
    id))

(define (bad-post-fragment id ptime section title url comm-url)
  `(form (@ (method "POST") (action "do-undelete")
            (id ,(conc "post-" id)) (class "bad-post")
            (hx-swap "outerHTML")  (hx-post "xdo-undelete"))
    (input (@ (type "submit") (name "submit") (class lsub) (value "Restore")))
    (div (@ (class "form-body"))
      ,(post-p-fragment id ptime section title url comm-url))
    (input (@ (type "hidden") (name "id") (value ,id)))))

(define (locked-post-fragment id ptime section title url comm-url mark)
  `(form (@ (method "POST") (action "do-locked")
            (id ,(conc "post-" id))
            (class ,(if (> mark 2) "protected-post" "locked-post"))
            (hx-swap "outerHTML")  (hx-post "xdo-locked"))
    (input (@ (type "submit") (name "submit") (class lsub) (value "Push")))
    (div (@ (class "form-body"))
      ,(post-p-fragment id ptime section title url comm-url))
    (input (@ (type "hidden") (name "id") (value ,id)))
    (input (@ (type "submit") (name "submit") (class rsub) (value "Unlock")))))

(define (marked-post-fragment id ptime section title url comm-url)
  `(form (@ (method "POST") (action "do-marked")
            (id ,(conc "post-" id)) (class "marked-post")
            (hx-swap "outerHTML")  (hx-post "xdo-marked"))
    (input (@ (type "submit") (name "submit") (class lsub) (value "Edit")))
    (div (@ (class "form-body"))
      ,(post-p-fragment id ptime section title url comm-url))
    (input (@ (type "hidden") (name "id") (value ,id)))
    (input (@ (type "submit") (name "submit") (class rsub) (value "Unmark")))))

(define (unmarked-post-fragment id ptime section title url comm-url)
  `(form (@ (method "POST") (action "do-unmarked")
            (id ,(conc "post-" id)) (class "unmarked-post")
            (hx-swap "outerHTML")  (hx-post "xdo-unmarked"))
    (input (@ (type "submit") (name "submit") (class lsub) (value "Mark")))
    (div (@ (class "form-body"))
      ,(post-p-fragment id ptime section title url comm-url))
    (input (@ (type "hidden") (name "id") (value ,id)))
    (input (@ (type "submit") (name "submit") (class rsub) (value "Delete")))))

(define (post-fragment id mark ptime section title url comm-url)
  (case mark
    ((0)    (unmarked-post-fragment id ptime section title url comm-url))
    ((1)    (marked-post-fragment   id ptime section title url comm-url))
    ((2 3)  (locked-post-fragment   id ptime section title url comm-url mark))
    (else   (bad-post-fragment      id ptime section title url comm-url))))

(define (post-htmx id)
  (htmx-output
    (query
      (map-rows* post-fragment)
      (sql db "SELECT id,mark,ptime,section,title,url,comment_url FROM gruik WHERE id=?;")
      id)))

(define (gruik-list-view title q)
  (html-output
    `(html
      (head
        (meta (@ (charset "utf-8")))
        (meta (@ (name "viewport")
                 (content "width=device-width, initial-scale=1")))
        (meta (@ (name "color-scheme") (content "light dark")))
        (title ,title)
        (script (@ (src "https://cdn.jsdelivr.net/npm/htmx.org@2.0.8/dist/htmx.min.js")) "")
        (style ,css-style))
      (body (h1 ,title)
        ,@(query
           (map-rows* post-fragment)
           (sql db q))
        (form (@ (method GET) (action "new") (id "load-new")
                 (hx-swap "outerHTML")  (hx-post "x-new"))
          ,(spinner)
          (input (@ (type "hidden") (name "last-id") (value
            ,(query fetch-value (sql db "SELECT MAX(id) FROM gruik;")))))
          (input (@ (type "submit") (name "submit") (value "Load"))))
))))

(define (new-fragment)
  (catch-up)
  (let* ((last-id (string->number (required-input-var "last-id")))
         (frags (query
                  (map-rows* post-fragment)
                  (sql db "SELECT id,mark,ptime,section,title,url,comment_url FROM gruik WHERE id > ? AND mark >= -5;")
                  last-id))
         (btn (if (null? frags) "Recheck" "More")))
  (htmx-output
    `(,@frags
        (form (@ (method GET) (action "new") (id "load-new")
                 (hx-swap "outerHTML")  (hx-post "x-new"))
          ,(spinner)
          (input (@ (type "hidden") (name "last-id") (value
            ,(query fetch-value (sql db "SELECT MAX(id) FROM gruik;")))))
          (input (@ (type "submit") (name "submit") (value ,btn))))
))))

(define (new-view)
  (redirect "/"))

(define (deleted-view)
  (catch-up)
  (gruik-list-view
    "Deleted gruiks"
    "SELECT id,mark,ptime,section,title,url,comment_url FROM gruik WHERE mark < 0 ORDER BY mtime;"))

(define (edit-view id)
  (let ((title (conc "Gruik #" id)))
    (html-output
      `(html
        (head
          (meta (@ (charset "utf-8")))
          (meta (@ (name "viewport")
                   (content "width=device-width, initial-scale=1")))
          (meta (@ (name "color-scheme") (content "light dark")))
          (title ,title)
          (script (@ (src "https://cdn.jsdelivr.net/npm/htmx.org@2.0.8/dist/htmx.min.js")) "")
          (style ,css-style))
        (body (h1 ,title)
          ,@(edit-post-fragment* id))))))

(define (main-view)
  (catch-up)
  (gruik-list-view
    "Latest gruiks"
    "SELECT id,mark,ptime,section,title,url,comment_url FROM gruik WHERE mark >= -5;"))

(define (db-push-gruik str-id)
  (let ((id (string->number str-id)))
    (with-transaction db
      (lambda ()
        (exec
          (sql db "INSERT INTO entry(url,type,description,notes,ctime,mtime,ptime,protected)
                   SELECT url,
                          CASE WHEN description IS NULL THEN NULL
                               WHEN substr(description,1,1)='<' THEN 'html'
                               WHEN substr(description,1,3)=' - '
                                 OR substr(description,1,3)=' + '
                                               THEN 'markdown-li'
                               ELSE 'text' END,
                          trim(description,char(10))||char(10),
                          trim(notes,char(10))||char(10),
                          stime,?,
                          CASE WHEN mark>=3 THEN ? ELSE NULL END,
                          CASE WHEN mark>=3 THEN 1 ELSE 0 END
                   FROM gruik
                   WHERE id=?;")
          (current-seconds)
          (current-seconds)
          id)
        (exec
          (sql db "INSERT OR IGNORE INTO tagrel(url_id,tag_id)
                   SELECT entry.id,tag_id
                   FROM gruik_tags LEFT OUTER JOIN gruik ON gruik_id=gruik.id
                                   LEFT OUTER JOIN entry ON gruik.url=entry.url
                   WHERE gruik_id=?;")
          id)
        (db-set-mark id 2 -10)
        (db-set-mark id 3 -10)))))

(define (db-set-mark id old-v new-v)
  (exec (sql db "UPDATE gruik SET mtime=?, mark=?, stime=?
                 WHERE mark=? AND id=?;")
        (current-seconds)
        new-v
        (if (= 1 new-v) (current-seconds) '())
        old-v
        id))

(define (xdo-edit)
  (let ((id (db-edit)))
    (post-htmx id)))

(define (do-locked)
  (let ((id     (required-input-var "id"))
        (submit (required-input-var "submit")))
    (cond
      ((string=? submit "Push")   (db-push-gruik id) (redirect "/"))
      ((string=? submit "Unlock") (db-set-mark id 2 1)
                                  (redirect (conc "/gruik/" id)))
      (else                       (bad-input "bad value for submit")))))

(define (xdo-locked)
  (let ((id     (required-input-var "id"))
        (submit (required-input-var "submit")))
    (cond
      ((string=? submit "Push")   (db-push-gruik id) (htmx-output '()))
      ((string=? submit "Unlock") (db-set-mark id 2 1) (post-htmx id))
      (else                       (bad-input "bad value for submit")))))

(define (do-marked)
  (let ((id     (required-input-var "id"))
        (submit (required-input-var "submit")))
    (cond
      ((string=? submit "Edit")   (redirect (conc "/gruik/" id)))
      ((string=? submit "Unmark") (db-set-mark id 1 0) (redirect "/"))
      (else                       (bad-input "bad value for submit")))))

(define (xdo-marked)
  (let ((id     (required-input-var "id"))
        (submit (required-input-var "submit")))
    (cond
      ((string=? submit "Edit")   (htmx-output
                                    (edit-post-fragment* (string->number id))))
      ((string=? submit "Unmark") (db-set-mark id 1 0) (post-htmx id))
      (else                       (bad-input "bad value for submit")))))

(define (do-undelete)
  (let ((id     (required-input-var "id"))
        (submit (required-input-var "submit")))
    (cond
      ((string=? submit "Restore") (db-set-mark id -10 0) (redirect "/"))
      (else                        (bad-input "bad value for submit")))))

(define (xdo-undelete)
  (let ((id     (required-input-var "id"))
        (submit (required-input-var "submit")))
    (cond
      ((string=? submit "Restore") (db-set-mark id -10 0) (htmx-output '()))
      (else                        (bad-input "bad value for submit")))))

(define (do-unmarked)
  (let ((id     (required-input-var "id"))
        (submit (required-input-var "submit")))
    (cond
      ((string=? submit "Mark")   (db-set-mark id 0  1)
                                  (auto-descr id)
                                  (redirect "/"))
      ((string=? submit "Delete") (db-set-mark id 0 -10) (redirect "/"))
      (else                       (bad-input "bad value for submit")))))

(define (xdo-unmarked)
  (let ((id     (required-input-var "id"))
        (submit (required-input-var "submit")))
    (cond
      ((string=? submit "Mark")   (db-set-mark id 0  1)
                                  (auto-descr id)
                                  (post-htmx id))
      ((string=? submit "Delete") (db-set-mark id 0 -10) (htmx-output '()))
      (else                       (bad-input "bad value for submit")))))


(define route-xdo-edit
  (preceded-by (char-seq "xdo-edit")
               (result xdo-edit)))
(define route-do-locked
  (preceded-by (char-seq "do-locked")
               (result do-locked)))
(define route-xdo-locked
  (preceded-by (char-seq "xdo-locked")
               (result xdo-locked)))
(define route-do-marked
  (preceded-by (char-seq "do-marked")
               (result do-marked)))
(define route-xdo-marked
  (preceded-by (char-seq "xdo-marked")
               (result xdo-marked)))
(define route-do-undelete
  (preceded-by (char-seq "do-undelete")
               (result do-undelete)))
(define route-xdo-undelete
  (preceded-by (char-seq "xdo-undelete")
               (result xdo-undelete)))
(define route-do-unmarked
  (preceded-by (char-seq "do-unmarked")
               (result do-unmarked)))
(define route-xdo-unmarked
  (preceded-by (char-seq "xdo-unmarked")
               (result xdo-unmarked)))
(define route-deleted
  (preceded-by (char-seq "deleted")
               (result deleted-view)))
(define route-new
  (preceded-by (char-seq "new")
               (result new-view)))
(define route-x-new
  (preceded-by (char-seq "x-new")
               (result new-fragment)))
(define route-spinner
  (preceded-by (char-seq "spinner")
               (result (lambda () (htmx-output (spinner))))))
(define route-edit
  (sequence* ((_  (char-seq "gruik/"))
              (id (as-string (one-or-more irc-digit))))
    (result (lambda () (edit-view (string->number id))))))
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
                   (list route-do-locked
                         route-do-marked
                         route-do-undelete
                         route-do-unmarked
                         route-xdo-edit
                         route-xdo-locked
                         route-xdo-marked
                         route-xdo-undelete
                         route-xdo-unmarked
                         route-deleted
                         route-edit
                         route-main
                         route-ok
                         route-new
                         route-spinner
                         route-x-new)))))

(let* ((uri (get-environment-variable "REQUEST_URI"))
       (_   (if uri uri (die "Missing $REQUEST_URI")))
       (fn  (parse router uri)))
  (if fn
    (fn)
    (debug-output)))
