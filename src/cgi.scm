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
  (chicken sort)
  (chicken string)
  (chicken time)
  (chicken time posix)
  comparse
  openssl ; must be above http-client
  http-client
  lowdown
  message-digest-byte-vector
  rss
  sha256-primitive
  sql-de-lite
  sxml-serializer)

(define sha-256 (sha256-primitive))
(define css-style #<<END-OF-CSS
* { box-sizing: border-box; }
h1 { text-align: center; }
nav ul { display: flex; justify-content: space-evenly; align-items: center; list-style-type: none; margin: 2ex 0; padding: 0; }
pre { overflow: auto; }
.form-body { overflow: auto; }
.bad-post { background: #fcc; }
.marked-post { background: #ccf; }
.locked-post { background: #cff; }
.protected-post { background: #cfc; }
form {
  position: relative;
  margin: 1rex 0;
  display: grid;
  gap: 0.5rex;
  transition: all 0.5s ease-in;
}
.sidenote {
  position: absolute;
  top: 1ex; right: 1ex;
  margin: 0;
  opacity: 0.6;
}
.lsub { width: 4.5rem; height: 3rem; }
.rsub { width: 4.5rem; height: 3rem; }
input[type=url] { display: block; width: 100%; }
textarea { display: block; max-width: 100%; }
.tag-list { column-width: 10rem; column-gap: 1rem; }
.tag-list label { display: block; }
span.ptime { font-size: 80%; }
span.section { font-size: 80%; }
a.section { font-size: 80%; }
span.title { font-weight: bold; display: block; }
span.taglist { font-weight: bold; font-size: 80%; }
span.hashid { font-size: 90%; opacity: 0.6; }
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
(define url-hdigit
  (any-of (preceded-by (is #\0) (result  0))
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
(define url-percent-escape
   (sequence* ((_ (is #\%))
               (h url-hdigit)
               (l url-hdigit))
     (result (integer->char (+ (* 16 h) l)))))
(define url-value
  (as-string
    (any-of (repeated (any-of url-percent-escape item) until: (is #\&))
            (repeated (any-of url-percent-escape item)))))
(define url-key
  (as-string (repeated item until: (is #\=))))
(define url-kv-pair
  (sequence* ((k url-key)
              (_ (is #\=))
              (v url-value)
              (_ (is #\&)))
    (result (list k (string-translate v "\r")))))
(define url-kv-pairs
  (zero-or-more url-kv-pair))
(define input-list
  (parse url-kv-pairs (string-append input-text "&")))
(define (input-var name)
  (let loop ((rest input-list))
    (cond ((null? rest) #f)
          ((string=? (caar rest) name) (cadar rest))
          (else (loop (cdr rest))))))
(define (optional-input-var name fallback)
  (let ((val (input-var name)))
    (if val val fallback)))
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
(define feed-root
  (let ((raw (get-environment-variable "FEED_ROOT")))
    (cond
      ((or (not raw) (zero? (string-length raw))) "")
      ((eqv? #\/ (string-ref raw (sub1 (string-length raw)))) raw)
      (else (string-append raw "/")))))

(define db (open-database db-name))
(exec (sql/transient db "PRAGMA foreign_keys = ON;
                         PRAGMA journal_mode = WAL;
                         PRAGMA synchronous = NORMAL;
                         PRAGMA busy_timeout = 5000;"))
(set-busy-handler! db (busy-timeout 10000))

(include "common.scm")

(unless (= 8 (db-version))
  (die "Unexpectad database version"))


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

(define (catch-up)
  (let* ((span (get-config "gruik-clean")))
    (when (number? span)
      (exec
        (sql db "DELETE FROM gruik
                 WHERE mark < 0 AND mtime < ?1
                   AND (lastseen IS NULL OR lastseen < ?1);")
        (- (current-seconds) span))))
  (let ((src-path (get-config "gruik-source")))
    (when src-path
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
                (loop (cadr rp))))))))))

(define (redirect location)
  (write-string "Status: 302\r\nLocation: ")
  (write-string (get-config/default "gruik-host" ""))
  (write-string (get-config/default "gruik-prefix" ""))
  (write-string location)
  (write-string "\r\n\r\n"))

(define (auto-descr id)
  (let ((row (query fetch-row
                    (sql db
                      (if (positive? id)
                          "SELECT section,url,comment_url FROM gruik
                           WHERE id=? AND COALESCE(description,'')='';"
                          "SELECT section,url,section_url FROM entry
                           WHERE protected=0 AND id=?
                             AND COALESCE(description,'')='';"))
                    (abs id))))
    (unless (null? row)
      (let* ((section (car row))
             (url     (cadr row))
             (dbcomm  (caddr row))
             (comm    (if (or (null? dbcomm) (string=? dbcomm ""))
                          (comment-link section url)
                          dbcomm)))
        (if comm
          (exec
            (sql db
              (if (positive? id)
                "UPDATE gruik
                 SET description=?,
                     notes=trim(notes||char(10)||?,char(10)),
                     comment_url=?
                 WHERE id=? AND COALESCE(description,'')='';"
                "UPDATE entry
                 SET description=?,
                     notes=trim(notes||char(10)||?,char(10)),
                     source_url=?
                 WHERE protected=0 AND id=? AND COALESCE(description,'')='';"))
            (conc " + [](" url ")\n(via [" section "](" comm ") sur #gcufeed)")
            comm
            comm
            (abs id))
          (exec
            (sql db
              (if (positive? id)
                "UPDATE gruik SET description=?
                 WHERE id=? AND COALESCE(description,'')='';"
                "UPDATE entry SET description=?
                 WHERE protected=0 AND id=? AND COALESCE(description,'')='';"))
            (conc " + [](" url ")\n(via " section " sur #gcufeed)")
            (abs id)))))))

(define (output-log-counts)
  (let ((ne (query fetch-value
                   (sql db "SELECT COUNT(*) FROM entry WHERE protected=0;")))
        (ng (query fetch-value
                   (sql db "SELECT COUNT(*) FROM gruik WHERE mark>0;"))))
    (write-line (conc (rfc-3339 (current-seconds)) "\t" ne "\t" ng))))
(define (log-counts)
  (let ((fname (get-environment-variable "NLOG")))
    (when fname
      (with-output-to-file fname output-log-counts #:append #:text))))

(define (spinner-bar x y height beg)
  `(rect (@ (x ,x) (y ,y) (width 15) (height ,height) (rx 6))
    (animate (@ (attributeName height) (begin ,beg) (dur "1s")
                (values "120;110;100;90;80;70;60;50;40;140;120")
                (calcMode linear) (repeatCount indefinite)))
    (animate (@ (attributeName y) (begin ,beg) (dur "1s")
                (values "10;15;20;25;30;35;40;45;50;0;10")
                (calcMode linear) (repeatCount indefinite)))))
(define (spinner-symbol)
  `(svg (@ (style "display: none") (xmlns "http://www.w3.org/2000/svg"))
    (symbol (@ (id "spinner") (viewBox "0 0 135 140"))
      ,(spinner-bar   0 10 120 "0.5s")
      ,(spinner-bar  30 10 120 "0.25s")
      ,(spinner-bar  60  0 140 "0s")
      ,(spinner-bar  90 10 120 "0.25s")
      ,(spinner-bar 120 10 120 "0.5s"))))
(define (spinner-ref)
  `(svg (@ (class spinner)) (use (@ (href "#spinner")) "")))

(define (post-p-fragment id ptime section title url comm-url tags)
  `(p
    (span (@ (class "ptime") (title ,id)) ,ptime)
    ,(if (null? comm-url)
         `(span (@ (class "section")) ,section)
         `(a (@ (href ,comm-url) (class "section")) ,section))
    ,@(if (or (null? tags) (string=? tags "")) '()
         `((span (@ (class "taglist")) ,tags)))
    (span (@ (class "title")) ,title)
    (a (@ (href ,url)) ,url)
    (span (@ (class "hashid"))
      "#" ,(substring (message-digest-string sha-256 url) 0 8))))

(define (domain-counts url)
  (and-let* ((i1     (substring-index "://" url))
             (i2     (substring-index "/" url (+ i1 3)))
             (s      (substring url i1 (add1 i2)))
             (domain (substring url (+ i1 3) i2)))
    (list domain
      (query fetch-value
             (sql db "SELECT COUNT(*) FROM entry WHERE instr(url,?)>0") s)
      (query fetch-value
             (sql db "SELECT COUNT(*) FROM gruik
                      WHERE mark>=0 AND instr(url,?)>0") s))))

(define (edit-post-fragment id ptime section title url comm-url mark notes description tags)
  `(form (@ (method "POST") (action "do-edit")
            (id ,(conc "post-" id)) (class "edit-post")
            (hx-swap "outerHTML")  (hx-post "xdo-edit"))
    (input (@ (type "submit") (name "submit") (class lsub) (value "Edit")))
    (div (@ (class "form-body"))
      ,(post-p-fragment id ptime section title url comm-url tags)
      ,@(let ((counts (domain-counts url)))
         (if (and counts (positive? (+ (cadr counts) (caddr counts) -1)))
           `((p "Entries and gruiks from "
                (a (@ (href ,(conc "domains/" (car counts))))
                   ,(car counts))
                ,(conc ": " (cadr counts) "+" (caddr counts))))
           '()))
      (p (label "URL:"
        (input (@ (type "url") (name "url") (value ,url)))))
      ,(if (positive? id)
        `(p ,(conc "Mark: " mark)
          (label (input (@ (type radio) (name mark) (value 0))) "Unmark")
          (label (input (@ (type radio) (name mark) (value 1) (checked)))
                 "Keep")
          (label (input (@ (type radio) (name mark) (value 2))) "Lock")
          (label (input (@ (type radio) (name mark) (value 3))) "Protect"))
        `(p (label "Protected: "
              (input (@ (type checkbox) (name protected) (value yes)
                        ,@(if (zero? mark) '() '((checked))))))))
      (pre (code ,notes))
      ,@(if (null? comm-url)
            `((p (label (input (@ (type checkbox) (name retry-comm) (value y)))
                               "Retry fetching comment URL")))
            '())
      (p (label "Comment URL:"
        (input (@ (type "url") (name "commenturl") (value ,comm-url)))))
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
              (sql db
                (if (positive? id)
                    "SELECT id,name,
                            EXISTS (SELECT * FROM gruik_tags
                                    WHERE gruik_id=? AND tag_id = tag.id)
                     FROM tag;"
                    "SELECT id,name,
                            EXISTS (SELECT * FROM tagrel
                                    WHERE url_id=? AND tag_id = tag.id)
                     FROM tag;"))
              (abs id)))))
    (input (@ (type "hidden") (name "id") (value ,id)))
    (input (@ (type "submit") (name "submit") (class rsub) (value "Cancel")))))

(define (edit-post-fragment* id)
  (query
    (map-rows* edit-post-fragment)
    (sql db
      (if (positive? id)
          "SELECT gruik.id,ptime,section,title,url,comment_url,mark,
                  notes,description,group_concat('#'||name,' ')
           FROM gruik LEFT OUTER JOIN gruik_tags ON gruik_id=gruik.id
                      LEFT OUTER JOIN tag ON tag_id=tag.id
           WHERE gruik.id=? GROUP BY gruik.id;"
          "SELECT -entry.id,
                  strftime('%Y.%m.%d %H:%M:%S', ctime, 'unixepoch') AS ptime,
                  COALESCE(source, 'Untracked Ien'),
                  COALESCE(title, ''), url, source_url, protected,
                  notes, description, group_concat('#'||name, ' ')
           FROM entry LEFT OUTER JOIN tagrel ON url_id=entry.id
                      LEFT OUTER JOIN tag ON tag_id=tag.id
           WHERE entry.id=? GROUP BY url_id;"))
    (abs id)))

(define (db-edit)
  (let ((id         (string->number (required-input-var "id")))
        (comm-url   (input-var "commenturl"))
        (main-url   (input-var "url"))
        (retry-comm (input-var "retry-comm")))
    (when (string=? "Edit" (required-input-var "submit"))
      (exec
        (sql/transient db
          (if (positive? id)
            "UPDATE gruik SET mtime=?,notes=trim(notes||char(10)||?,char(10)),
                              description=?,mark=?,comment_url=?,
                              url=COALESCE(?,url)
             WHERE (mark=1 OR mark=2) AND id=?;"
            "UPDATE entry SET mtime=?,notes=trim(notes||char(10)||?,char(10)),
                              description=?,protected=?,source_url=?,
                              url=COALESCE(?,url)
             WHERE protected=0 AND id=?;"))
        (current-seconds)
        (required-input-var "notes")
        (if retry-comm "" (required-input-var "description"))
        (if (positive? id) (string->number (required-input-var "mark")) 0)
        (if (and comm-url (not (string=? comm-url ""))) comm-url '())
        (if (and main-url (not (string=? main-url ""))) main-url '())
        (abs id))
      (when retry-comm (auto-descr id))
      (let* ((n-tags  (query fetch-value (sql db "SELECT MAX(id) FROM tag")))
             (tags    (make-vector (+ 1 n-tags) 0))
             (add-tag (sql db
                        (if (positive? id)
                          "INSERT INTO gruik_tags(gruik_id,tag_id)
                           VALUES (?,?);"
                          "INSERT INTO tagrel(url_id,tag_id)
                           VALUES (?,?);")))
             (del-tag (sql db
                        (if (positive? id)
                          "DELETE FROM gruik_tags
                           WHERE gruik_id=? AND tag_id=?;"
                          "DELETE FROM tagrel
                           WHERE url_id=? AND tag_id=?;"))))
        (let loop ((var input-list))
          (unless (null? var)
            (when (string=? (caar var) "tags")
              (vector-set! tags (string->number (cadar var)) 1))
            (loop (cdr var))))
        (query
          (for-each-row*
            (lambda (tid) (vector-set! tags tid (- (vector-ref tags tid) 1))))
          (sql db
            (if (positive? id)
              "SELECT tag_id FROM gruik_tags WHERE gruik_id=?;"
              "SELECT tag_id FROM tagrel WHERE url_id=?;"))
          (abs id))
        (let loop ((tid n-tags))
          (unless (= 0 tid)
            (case (vector-ref tags tid)
              ((1)  (exec add-tag (abs id) tid))
              ((-1) (exec del-tag (abs id) tid)))
            (loop (- tid 1))))))
    id))

(define (post-fragment-id id)
  (if (positive? id) (conc "post-" id) (conc "entry" id)))

(define (post-fragment id mark ptime section title url comm-url tags . details)
  (let* ((data (case mark
                 ((0)  '("unmarked" "unmarked"  "Mark"    "Delete"))
                 ((1)  '("marked"   "marked"    "Edit"    "Unmark"))
                 ((2)  '("locked"   "locked"    "Push"    "Edit"))
                 ((3)  '("locked"   "protected" "Push"    "Edit"))
                 ((10) '("ien"      "locked"    "Protect" "Edit"))
                 ((11) '("ien"      "protected" #f        "Unprotect"))
                 (else `("undelete" "bad"       "Restore" ,(if (<= -5 mark 0)
                                                              "Hide" #f)))))
         (action (car data))
         (class  (cadr data))
         (llabel (caddr data))
         (rlabel (cadddr data)))
  `(form (@ (method "POST") (action ,(conc "do-" action))
            (id ,(post-fragment-id id))
            (class ,(conc class "-post"))
            (hx-swap "outerHTML") (hx-post ,(conc "xdo-" action)))
    ,@(if (or llabel rlabel)
          `((input (@ (type "hidden") (name "id") (value ,id))))
          '())
    ,@(if llabel
          `((input (@ (type "submit") (name "submit")
                      (class lsub) (value ,llabel))))
          '())
    (div (@ (class "form-body"))
      ,(post-p-fragment id ptime section title url comm-url tags)
      ,@(if (or (null? details) (string=? (car details) ""))
            '() `((pre (code ,(car details))))))
    ,@(if rlabel
          `(,@(if (<= -5 mark -1)
                  `((input (@ (type "hidden") (name "from") (value ,mark))))
                  '())
            (input (@ (type "submit") (name "submit")
                      (class rsub) (value ,rlabel))))
          '()))))

(define (post-htmx id)
  (htmx-output
    (query
      (map-rows* post-fragment)
      (sql db
        (if (positive? id)
          "SELECT gruik.id,mark,ptime,section,title,url,comment_url,
                  group_concat('#'||name,' ')
           FROM gruik LEFT OUTER JOIN gruik_tags ON gruik_id=gruik.id
                      LEFT OUTER JOIN tag ON tag_id=tag.id
           WHERE gruik.id=? GROUP BY gruik.id;"
          "SELECT -entry.id,(CASE WHEN protected=0 THEN 10 ELSE 11 END),
                  strftime('%Y.%m.%d %H:%M:%S',ctime,'unixepoch') AS ptime,
                  COALESCE(source,'Untracked Ien'),
                  COALESCE(title,''),url,source_url,
                  group_concat('#'||name,' ')
           FROM entry LEFT OUTER JOIN tagrel ON url_id=entry.id
                      LEFT OUTER JOIN tag ON tag_id=tag.id
           WHERE entry.id=? GROUP BY entry.id;"))
      (abs id))))

(define (gruik-list-view title row->fragment footer q . args)
  (html-output
    `(html
      (head
        (base (@ (href ,(conc (get-config/default "gruik-prefix" "") "/"))))
        (meta (@ (charset "utf-8")))
        (meta (@ (name "viewport")
                 (content "width=device-width, initial-scale=1")))
        (meta (@ (name "color-scheme") (content "light dark")))
        (title ,title)
        (script (@ (src "https://cdn.jsdelivr.net/npm/htmx.org@2.0.8/dist/htmx.min.js")) "")
        (style ,css-style))
      (body
        ,(spinner-symbol)
        (h1 ,title)
        (nav (ul
          (li (a (@ (href "./")) "Latest gruiks"))
          (li (a (@ (href "deleted")) "Deleted gruiks"))
          (li (a (@ (href "no-comm")) "Sourceless gruiks"))))
        ,@(apply query
           (map-rows* row->fragment)
           (sql db q)
           args)
        ,@footer))))

(define (new-fragment)
  (catch-up)
  (let* ((last-id   (string->number (required-input-var "last-id")))
         (last-time (string->number (required-input-var "last-time")))
         (n-new     0)
         (n-upd     0)
         (n-del     0)
         (frags (query
                  (map-rows*
                    (lambda (id mark ptime section title url comm-url tags)
                      (let ((base (post-fragment id mark ptime section
                                                 title url comm-url tags)))
                        (cond
                          ((> id last-id)
                            (set! n-new (add1 n-new))
                            base)
                          ((>= mark -5)
                            (set! n-upd (add1 n-upd))
                            `(form (@ (hx-swap-oob "true") ,@(cdadr base))
                                   ,@(cddr base)))
                          (else
                            (set! n-del (add1 n-del))
                            `(form (@ (hx-swap-oob "delete")
                                      (id ,(post-fragment-id id)))
                                   ""))))))
                  (sql db "SELECT gruik.id,mark,ptime,section,title,url,
                                  comment_url,group_concat('#'||name,' ')
                           FROM gruik LEFT OUTER JOIN gruik_tags
                                                      ON gruik_id=gruik.id
                                      LEFT OUTER JOIN tag ON tag_id=tag.id
                           WHERE mtime > ? AND (gruik.id <= ? OR mark >= -5)
                           GROUP BY gruik.id;")
                  last-time last-id))
         (btn (if (null? frags) "Recheck" "More")))
  (htmx-output
    `(,@frags
        (form (@ (method GET) (action "new") (id "load-new")
                 (hx-swap "outerHTML")  (hx-post "x-new"))
          ,@(if (positive? (+ n-new n-upd n-del))
              `((p (@ (class sidenote))
                   ,(if (positive? n-new) (conc "+" n-new) "")
                   ,(if (positive? n-upd) (conc "~" n-upd) "")
                   ,(if (positive? n-del) (conc "−" n-del) "")))
              '())
          ,(spinner-ref)
          (input (@ (type "hidden") (name "last-time")
                    (value ,(current-seconds))))
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
    post-fragment
    '()
    "SELECT gruik.id,mark,ptime,section,title,url,comment_url,
            group_concat('#'||name,' ')
     FROM gruik LEFT OUTER JOIN gruik_tags ON gruik_id=gruik.id
                LEFT OUTER JOIN tag ON tag_id=tag.id
     WHERE mark < 0 GROUP BY gruik.id ORDER BY mtime DESC;"))

(define (edit-view id)
  (let ((title (conc (if (positive? id) "Gruik #" "Ien #") (abs id))))
    (html-output
      `(html
        (head
          (base (@ (href ,(conc (get-config/default "gruik-prefix" "") "/"))))
          (meta (@ (charset "utf-8")))
          (meta (@ (name "viewport")
                   (content "width=device-width, initial-scale=1")))
          (meta (@ (name "color-scheme") (content "light dark")))
          (title ,title)
          (script (@ (src "https://cdn.jsdelivr.net/npm/htmx.org@2.0.8/dist/htmx.min.js")) "")
          (style ,css-style))
        (body
          ,(spinner-symbol)
          (h1 ,title)
          ,@(edit-post-fragment* id))))))

(define (feed-view id)
  (let ((row (query fetch-row
                    (sql/transient db "SELECT mtime,title,url,selector
                                       FROM feed WHERE id=?;")
                    id)))
    (if (null? row)
        (write-string "Status: 404\r\n\r\n")
        (let ((mtime    (car    row))
              (title    (cadr   row))
              (self-url (caddr  row))
              (selector (cadddr row)))
          (write-string "Content-Type: application/atom+xml\r\n\r\n")
          (write-feed mtime title self-url (feed-rows selector))))))

(define (main-view)
  (catch-up)
  (gruik-list-view
    "Latest gruiks"
    post-fragment
    `((form (@ (method GET) (action "new") (id "load-new")
               (hx-swap "outerHTML")  (hx-post "x-new"))
        ,(spinner-ref)
        (input (@ (type "hidden") (name "last-time")
                  (value ,(current-seconds))))
        (input (@ (type "hidden") (name "last-id") (value
          ,(query fetch-value (sql db "SELECT MAX(id) FROM gruik;")))))
        (input (@ (type "submit") (name "submit") (value "Load")))))
    "SELECT gruik.id,mark,ptime,section,title,url,comment_url,
            group_concat('#'||name,' ')
     FROM gruik LEFT OUTER JOIN gruik_tags ON gruik_id=gruik.id
                LEFT OUTER JOIN tag ON tag_id=tag.id
     WHERE mark >= -5 GROUP BY gruik.id;"))

(define (view-domain-search q)
  (gruik-list-view
    (conc "Domain " q)
    post-fragment
    '()
    "SELECT gruik.id,mark,ptime,section,title,url,comment_url,
            group_concat('#'||name,' '),COALESCE(description,notes)
     FROM gruik LEFT OUTER JOIN gruik_tags ON gruik_id=gruik.id
                LEFT OUTER JOIN tag ON tag_id=tag.id
     WHERE instr(url,?1)>0 GROUP BY gruik.id
     UNION ALL
     SELECT -entry.id,(CASE WHEN protected=0 THEN 10 ELSE 11 END),
            strftime('%Y.%m.%d %H:%M:%S',ctime,'unixepoch') AS ptime,
            COALESCE(source,'Untracked Ien'),
            COALESCE(title,''),url,source_url,
            group_concat('#'||name,' '),COALESCE(description,notes)
     FROM entry LEFT OUTER JOIN tagrel ON url_id=entry.id
                LEFT OUTER JOIN tag ON tag_id=tag.id
     WHERE instr(url,?1)>0 GROUP BY url_id
     ORDER BY ptime"
    (conc "://" q "/")))

(define (view-no-comm)
  (catch-up)
  (gruik-list-view
    "Marked gruiks without comment URL"
    post-fragment
    '()
    "SELECT gruik.id,mark,ptime,section,title,url,comment_url,
            group_concat('#'||name,' ')
     FROM gruik LEFT OUTER JOIN gruik_tags ON gruik_id=gruik.id
                LEFT OUTER JOIN tag ON tag_id=tag.id
     WHERE mark >= 1 AND COALESCE(comment_url,'') = '' GROUP BY gruik.id;"))

(define (view-selection id)
  (let ((row (query fetch-row
                    (sql/transient db "SELECT name,text
                                       FROM selector WHERE id=?;")
                    id)))
    (if (null? row)
        (write-string "Status: 404\r\n\r\n")
        (gruik-list-view
          (conc "Selection #" id ": " (car row))
          post-fragment
          '()
          (conc
            "SELECT -entry.id,(CASE WHEN protected=0 THEN 10 ELSE 11 END),
                    strftime('%Y.%m.%d %H:%M:%S',ctime,'unixepoch') AS ptime,
                    COALESCE(source,'Untracked Ien'),
                    COALESCE(title,''),url,source_url,
                    group_concat('#'||name,' '),COALESCE(description,notes)
             FROM entry LEFT OUTER JOIN tagrel ON url_id=entry.id
                        LEFT OUTER JOIN tag ON tag_id=tag.id "
            (cadr row)
            "GROUP BY url_id ORDER BY ptime")))))

(define (view-url-search op q)
  (gruik-list-view
    (conc "Gruks " op " " q)
    post-fragment
    '()
    (conc "SELECT gruik.id,mark,ptime,section,title,url,comment_url,
                  group_concat('#'||name,' '),COALESCE(description,notes)
           FROM gruik LEFT OUTER JOIN gruik_tags ON gruik_id=gruik.id
                      LEFT OUTER JOIN tag ON tag_id=tag.id
           WHERE url " op " ? GROUP BY gruik.id;")
    q))

(define (db-push-gruik id)
  (with-transaction db
    (lambda ()
      (exec
        (sql db "INSERT INTO entry(url,type,description,notes,
                                   title,source,source_url,
                                   ctime,mtime,ptime,protected)
                 SELECT url,
                        CASE WHEN description IS NULL THEN NULL
                             WHEN substr(description,1,1)='<' THEN 'html'
                             WHEN substr(description,1,3)=' - '
                               OR substr(description,1,3)=' + '
                                             THEN 'markdown-li'
                             ELSE 'text' END,
                        trim(description,char(10))||char(10),
                        trim(notes,char(10))||char(10),
                        title,section,comment_url,
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
      (db-set-mark id 3 -10)))
  (log-counts))

(define (db-set-mark id old-v new-v)
  (exec (sql db "UPDATE gruik SET mtime=?, mark=?, stime=COALESCE(stime,?)
                 WHERE mark=? AND id=?;")
        (current-seconds)
        new-v
        (if (= 1 new-v) (current-seconds) '())
        old-v
        id))

(define (db-set-protected id old-p new-p)
  (exec (sql db "UPDATE entry SET mtime=?, ptime=?, protected=?
                 WHERE protected=? AND id=?;")
        (current-seconds)
        (if (zero? new-p) '() (current-seconds))
        new-p
        old-p
        (- id)))

(define (db-sel-count id text name)
  (list id text name
    (query fetch-value
           (sql db (string-append "SELECT COUNT(id) FROM entry " text ";")))))
(define (db-sel-counts)
  (query (map-rows* db-sel-count)
         (sql db "SELECT id,text,name FROM selector ORDER BY id DESC;")))
(define (diff-sel-counts before after)
  (let loop ((rest-before before) (rest-after after) (acc '()))
    (cond
      ((and (null? rest-before) (null? rest-after)) acc)
      ((null? rest-before)
        (loop rest-before
              (cdr rest-after)
              (cons (list 0 (conc "extra after: " (cadar rest-after)) 0 0)
                    acc)))
      ((null? rest-after)
        (loop (cdr rest-before)
              rest-after
              (cons (list 0 (conc "extra before: " (cadar rest-before)) 0 0)
                    acc)))
      ((not (= (caar rest-before) (caar rest-after)))
        (loop (cdr rest-before)
              (cdr rest-after)
              (cons (list 0
                          (conc "id mismatch: "
                                (caar rest-before) " / " (caar rest-after))
                          0 0)
                    acc)))
      ((not (string=? (cadar rest-before) (cadar rest-after)))
        (loop (cdr rest-before)
              (cdr rest-after)
              (cons (list 0
                          (conc "text mismatch: "
                                (cadar rest-before) " / " (cadar rest-after))
                          0 0)
                    acc)))
      ((not (string=? (caddar rest-before) (caddar rest-after)))
        (loop (cdr rest-before)
              (cdr rest-after)
              (cons (list 0
                          (conc "name mismatch: "
                                (caddar rest-before) " / " (caddar rest-after))
                          0 0)
                    acc)))
      (else
        (let ((n-before (car (cdddar rest-before)))
              (n-after  (car (cdddar rest-after))))
          (loop (cdr rest-before)
                (cdr rest-after)
                (if (= n-before n-after)
                    acc
                    (cons (list (caar rest-before)
                                (cadar rest-before)
                                (caddar rest-before)
                                n-after
                                (- n-after n-before))
                          acc))))))))
(define (fragment-diff-sel-counts before after)
  (let ((diff (diff-sel-counts before after)))
    (if (null? diff) '()
      `((table
        ,@(map (lambda (line)
                 `(tr (td (a (@ (href ,(conc "selection/" (car line))))
                             ,(conc "Selection #" (car line))))
                      (td (@ (title ,(list-ref line 1))) ,(list-ref line 2))
                      (td ,(->string (list-ref line 3)))
                      (td ,(conc (if (positive? (list-ref line 4)) "(+" "(")
                                 (list-ref line 4) ")"))))
               diff))))))

(define (feed-sig-base)
  (query (map-rows (lambda (row) (append row (build-signature (caddr row)))))
         (sql db "SELECT id,title,selector FROM feed WHERE active=1;")))
(define (linked-ien n)
  `(a (@ (href ,(conc "ien/" n))) ,(conc "item #" n)))
(define (fragment-sig-diff id title diff)
  `((p ,(conc "Feed #" id ": " title))
    (ul ,@(map (lambda (hunk) (cond
                 ((eqv? (car hunk) 'add)
                   `(li "added " ,(linked-ien (cadr hunk))
                        ,(conc " at " (rfc-3339 (caddr hunk)))))
                 ((eqv? (car hunk) 'del)
                   `(li "removed " ,(linked-ien (cadr hunk))
                        ,(conc " at " (rfc-3339 (caddr hunk)))))
                 ((eqv? (car hunk) 'chg)
                   `(li "updated " ,(linked-ien (cadr hunk))
                        ,(conc " from " (rfc-3339 (caddr hunk))
                               " to " (rfc-3339 (cadddr hunk)))))
                 (else `(li ,(conc "malformed hunk: " hunk)))))
               diff))))
(define (update-feed id)
  (exec (sql/transient db "UPDATE feed SET mtime=? WHERE id=?;")
        (current-seconds)
        id)
  (query (for-each-row*
           (lambda (filename mtime title self-url selector)
             (let ((rows (feed-rows selector)))
               (unless (null? rows)
                 (with-output-to-file (string-append feed-root filename)
                   (lambda ()
                     (write-feed
                       (if (null? mtime) (list-ref (car rows) 7) mtime)
                       title
                       self-url
                       rows)))))))
         (sql/transient db
           "SELECT filename,mtime,title,url,selector FROM feed WHERE id=?;")
         id))
(define (fragment-diff-feed* base-sig)
  (let ((id       (car   base-sig))
        (title    (cadr  base-sig))
        (selector (caddr base-sig))
        (old-sig  (cdddr base-sig)))
    (let ((diff (diff-signature old-sig (build-signature selector))))
      (if (null? diff)
          '()
          (begin
            (update-feed id)
            (fragment-sig-diff id title diff))))))
(define (fragment-diff-feed base-sigs)
  (join (map fragment-diff-feed* base-sigs)))

(define (fragment-push-report frag-diff-sel frag-diff-sig)
  (if (and (null? frag-diff-sel) (null? frag-diff-sig))
      '()
      `(form
        (div (@ (class "form-body")) ,@frag-diff-sel ,@frag-diff-sig)
        (button (@ (class rsub) (onclick "this.closest('form').remove()"))
          "Dismiss"))))

(define (htmx-push-gruik id)
  (let ((before (db-sel-counts))
        (base-sigs (feed-sig-base)))
    (db-push-gruik id)
    (htmx-output
      (fragment-push-report
        (fragment-diff-sel-counts before (db-sel-counts))
        (fragment-diff-feed base-sigs)))))

(define (xdo-edit)
  (let ((id (db-edit)))
    (post-htmx id)))

(define (do-ien htmx?)
  (let ((id     (string->number (required-input-var "id")))
        (submit (required-input-var "submit")))
    (cond
      ((positive? id) (bad-input "bad value for id"))
      ((string=? submit "Edit")
        (if htmx? (htmx-output (edit-post-fragment* id))
                  (redirect (conc "/ien/" (- id)))))
      ((string=? submit "Protect")
        (db-set-protected id 0 1)
        (if htmx? (post-htmx id)
                  (redirect (conc "/ien/" (- id)))))
      ((string=? submit "Unprotect")
        (db-set-protected id 1 0)
        (if htmx? (post-htmx id)
                  (redirect (conc "/ien/" (- id)))))
      (else (bad-input "bad value for submit")))))

(define (do-locked htmx?)
  (let ((id     (string->number (required-input-var "id")))
        (submit (required-input-var "submit")))
    (cond
      ((string=? submit "Push")
        (if htmx? (htmx-push-gruik id)
                  (begin (db-push-gruik id) (redirect "/"))))
      ((string=? submit "Edit")
        (if htmx? (htmx-output (edit-post-fragment* id))
                  (redirect (conc "/gruik/" id))))
      (else (bad-input "bad value for submit")))))

(define (do-marked htmx?)
  (let ((id     (string->number (required-input-var "id")))
        (submit (required-input-var "submit")))
    (cond
      ((string=? submit "Edit")
        (if htmx? (htmx-output (edit-post-fragment* id))
                  (redirect (conc "/gruik/" id))))
      ((string=? submit "Unmark")
        (db-set-mark id 1 0)
        (log-counts)
        (if htmx? (post-htmx id) (redirect "/")))
      (else (bad-input "bad value for submit")))))

(define (do-undelete htmx?)
  (let ((id      (string->number (required-input-var "id")))
        (oldmark (string->number (optional-input-var "from" "")))
        (submit  (required-input-var "submit")))
    (cond
      ((and oldmark (<= -5 oldmark -1))
        (cond
          ((string=? submit "Restore")
            (db-set-mark id oldmark 0)
            (if htmx? (post-htmx id)
                      (redirect (conc "/gruik/" id))))
          ((string=? submit "Hide")
            (db-set-mark id oldmark -10)
            (if htmx? (htmx-output '()) (redirect "/")))
          (else (bad-input "bad value for submit"))))
      ((string=? submit "Restore")
        (db-set-mark id -10 0)
        (if htmx? (htmx-output '()) (redirect "/")))
      (else (bad-input "bad value for submit")))))

(define (do-unmarked htmx?)
  (let ((id     (string->number (required-input-var "id")))
        (submit (required-input-var "submit")))
    (cond
      ((string=? submit "Mark")
        (db-set-mark id 0 1)
        (log-counts)
        (auto-descr id)
        (if htmx? (post-htmx id) (redirect "/")))
      ((string=? submit "Delete")
        (db-set-mark id 0 -10)
        (if htmx? (htmx-output '()) (redirect "/")))
      (else (bad-input "bad value for submit")))))

(define route-xdo-edit
  (preceded-by (any-of (char-seq "xdo-edit")
                       (char-seq "gruik/xdo-edit")
                       (char-seq "ien/xdo-edit"))
               (result xdo-edit)))
(define route-do-ien
  (sequence* ((x? (maybe (is #\x)))
              (_  (char-seq "do-ien")))
    (result (lambda () (do-ien x?)))))
(define route-do-locked
  (sequence* ((x? (maybe (is #\x)))
              (_  (char-seq "do-locked")))
    (result (lambda () (do-locked x?)))))
(define route-do-marked
  (sequence* ((x? (maybe (is #\x)))
              (_  (char-seq "do-marked")))
    (result (lambda () (do-marked x?)))))
(define route-do-undelete
  (sequence* ((x? (maybe (is #\x)))
              (_  (char-seq "do-undelete")))
    (result (lambda () (do-undelete x?)))))
(define route-do-unmarked
  (sequence* ((x? (maybe (is #\x)))
              (_  (char-seq "do-unmarked")))
    (result (lambda () (do-unmarked x?)))))
(define route-deleted
  (preceded-by (char-seq "deleted")
               (result deleted-view)))
(define route-feed
  (sequence* ((_  (char-seq "feed/"))
              (id (as-string (one-or-more irc-digit)))
              (_  (char-seq ".atom")))
    (result (lambda () (feed-view (string->number id))))))
(define route-new
  (preceded-by (char-seq "new")
               (result new-view)))
(define route-x-new
  (preceded-by (char-seq "x-new")
               (result new-fragment)))
(define route-domain-search
  (sequence* ((_ (char-seq "domains/"))
              (q (as-string (repeated item))))
    (result (lambda () (view-domain-search q)))))
(define route-no-comm
  (preceded-by (char-seq "no-comm")
               (result view-no-comm)))
(define route-selection
  (sequence* ((_  (char-seq "selection/"))
              (id (as-string (one-or-more irc-digit))))
    (result (lambda () (view-selection (string->number id))))))
(define route-url-search
  (sequence* ((_  (char-seq "url?"))
              (op (any-of (char-seq "glob")
                          (char-seq "like")
                          (char-seq "regexp")))
              (_  (is #\=))
              (q  url-value))
    (result (lambda () (view-url-search op q)))))
(define route-edit-gruik
  (sequence* ((_  (char-seq "gruik/"))
              (id (as-string (one-or-more irc-digit))))
    (result (lambda () (edit-view (string->number id))))))
(define route-edit-ien
  (sequence* ((_  (char-seq "ien/"))
              (id (as-string (one-or-more irc-digit))))
    (result (lambda () (edit-view (- (string->number id)))))))
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
                   (list route-do-ien
                         route-do-locked
                         route-do-marked
                         route-do-undelete
                         route-do-unmarked
                         route-domain-search
                         route-xdo-edit
                         route-deleted
                         route-edit-gruik
                         route-edit-ien
                         route-feed
                         route-main
                         route-ok
                         route-new
                         route-no-comm
                         route-selection
                         route-url-search
                         route-x-new)))))

(let* ((uri (get-environment-variable "REQUEST_URI"))
       (_   (if uri uri (die "Missing $REQUEST_URI")))
       (fn  (parse router uri)))
  (if fn
    (fn)
    (debug-output)))
