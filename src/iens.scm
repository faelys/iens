; Copyright (c) 2023-2024, Natacha Porté
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

(import (chicken condition)
        (chicken file)
        (chicken file posix)
        (chicken io)
        (chicken process)
        (chicken process signal)
        (chicken process-context)
        (chicken sort)
        (chicken string)
        (chicken time)
        (chicken time posix)
        breadline
        breadline-scheme-completion
        lowdown
        sql-de-lite
        srfi-1
        sxml-serializer)

(define (starts-with? maybe-prefix s)
  (substring=? s maybe-prefix 0 0 (string-length maybe-prefix)))

(define (ends-with? maybe-suffix s)
  (let ((ls  (string-length s))
        (lms (string-length maybe-suffix)))
  (and (>= ls lms)
       (substring=? s maybe-suffix (- ls lms)))))

(define (time->rfc-3339 time)
  (let ((time-str (time->string time "%FT%T%z")))
    (assert (= 24 (string-length time-str)))
    (if (equal? "0000" (substring time-str 20))
        (string-append (substring time-str 0 19) "Z")
        (string-append (substring time-str 0 22)
                       ":"
                       (substring time-str 22)))))

(define (rfc-3339-local seconds)
  (time->rfc-3339 (seconds->local-time seconds)))
(define (rfc-3339-utc seconds)
  (time->rfc-3339 (seconds->utc-time seconds)))
(define rfc-3339 rfc-3339-local)

(define (terminate-line line)
  (let ((l (string-length line)))
    (if (or (zero? l)
            (eqv? (string-ref line (sub1 l)) #\newline))
        line
        (string-append line "\n"))))

(define cmd-list '())

(define-syntax defcmd
  (syntax-rules ()
    ((defcmd (name . args) str first . rest)
      (begin
        (set! cmd-list (cons (list (symbol->string 'name) str first) cmd-list))
        (define (name . args) . rest)))))

(define vt100-entry-header "\033[34m")
(define vt100-reset        "\033[0m")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command-Line Processing

(define db-filename #f)
(define arg-replay #f)

(let ((arg-list (command-line-arguments)))
  (when (>= (length arg-list) 2) (set! arg-replay (cadr arg-list)))
  (when (>= (length arg-list) 1) (set! db-filename (car arg-list))))

;;;;;;;;;;;;;
;; Tracing

(define trace-port #f)
(define display-trace #t)

(define (trace obj)
  (when display-trace
    (write obj)
    (newline))
  (when trace-port
    (write obj trace-port)
    (newline trace-port)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Persistent Storage

(define db-name
  (if db-filename db-filename "iens.sqlite"))

(define db
  (open-database db-name))
(write-line (conc "Using database " db-name " with SQLite " library-version))
(exec (sql db "PRAGMA foreign_keys = ON;"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database Creation/Migration

(define schema-version 1)

(when (null? (schema db))
  (write-line (conc "Initializing database with schema v" schema-version))
  (for-each
    (lambda (s) (exec (sql/transient db s)))
    (list "CREATE TABLE config (key TEXT PRIMARY KEY, val);"
          (conc "INSERT INTO config(key, val) VALUES "
                "('schema-version'," schema-version ");")
          (conc "CREATE TABLE tag (id INTEGER PRIMARY KEY, "
                "name TEXT NOT NULL, auto INTEGER DEFAULT 0);")
          (conc "CREATE TABLE entry (id INTEGER PRIMARY KEY, "
                "url TEXT NOT NULL, type TEXT, description TEXT, notes TEXT, "
                "protected INTEGER DEFAULT 0, ptime INTEGER, "
                "ctime INTEGER NOT NULL DEFAULT CURRENT_TIMESTAMP, "
                "mtime INTEGER NOT NULL DEFAULT CURRENT_TIMESTAMP);")
          (conc "CREATE TABLE tagrel "
                "(url_id REFERENCES entry(id) "
                "ON UPDATE CASCADE ON DELETE CASCADE, "
                "tag_id REFERENCES tag(id) "
                "ON UPDATE CASCADE ON DELETE CASCADE);")
          (conc "CREATE TABLE feed ("
                "id INTEGER PRIMARY KEY, filename TEXT NOT NULL, "
                "url TEXT NOT NULL, selector TEXT NOT NULL, "
                "title TEXT NOT NULL, active INTEGER NOT NULL DEFAULT 1);")
          "CREATE INDEX i_mtime ON entry(mtime);"
          "CREATE INDEX i_pmtime ON entry(protected,mtime);"
          "CREATE UNIQUE INDEX i_url ON entry(url);"
          "CREATE UNIQUE INDEX i_tag ON tag(name);"
          "CREATE UNIQUE INDEX i_rel0 ON tagrel(url_id,tag_id);"
          "CREATE INDEX i_rel1 ON tagrel(url_id);"
          "CREATE INDEX i_rel2 ON tagrel(tag_id);")))

;;;;;;;;;;;;;;;;;;
;; Configuration

(define config-author-name #f)
(define config-author-email #f)
(define config-author-uri #f)
(define config-editor #f)
(define config-entry-id-prefix "")
(define config-verbose #f)

(define default-editor
  (let ((term   (get-environment-variable "TERM"))
        (visual (get-environment-variable "VISUAL"))
        (editor (get-environment-variable "EDITOR"))
        (fallback "vi"))
    (cond
      ((and visual term (not (equal? "dumb" term))) visual)
      (editor editor)
      (else fallback))))

(define (get-config key)
  (query fetch-value (sql db "SELECT val FROM config WHERE key = ?;") key))

(define (get-config/default key default-value)
  (let ((result (get-config key)))
    (if result
        result
        default-value)))

(define (string->filename data)
  (cond ((not data) #f)
        ((starts-with? "~/" data)
          (string-append (get-environment-variable "HOME")
                         (substring data 1)))
        (else data)))

(define (read-config!)
  (set! display-trace  (not (zero? (get-config/default "display-trace" 0))))
  (set! config-verbose (not (zero? (get-config/default "verbose" 0))))
  (set! rfc-3339        (if (zero? (get-config/default "local-time" 1))
                            rfc-3339-utc rfc-3339-local))
  (set! config-author-name  (get-config "author-name"))
  (set! config-author-email (get-config "author-email"))
  (set! config-author-uri   (get-config "author-uri"))
  (set! config-editor       (get-config/default "editor" default-editor))
  (set! config-entry-id-prefix (get-config/default "entry-id-prefix" ""))
  (let ((trace-filename (get-config "trace")))
    (when trace-port (close-output-port trace-port))
    (set! trace-port
      (if trace-filename
          (open-output-file (string->filename trace-filename) #:text #:append)
          #f)))
  (history-file (string->filename (get-config "histfile"))))

(read-config!)

(defcmd (print-config . args)
  "[key ...]" "Print configuration"
  (if (null? args)
      (query
        (for-each-row*
          (lambda (key val) (write-line (conc key ": " val))))
        (sql db "SELECT key,val FROM config ORDER BY key;"))
      (let loop ((todo args))
        (unless (null? todo)
          (write-line (conc (car todo) ": " (get-config (car todo))))
          (loop (cdr todo))))))

(defcmd (set-config key val)
  "key value" "Set configuration variable"
  (trace `(set-config ,key ,val))
  (exec (sql db "INSERT OR REPLACE INTO config VALUES (?,?);") key val)
  (read-config!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configurable Query Selectors

(when (= 1 (get-config "schema-version"))
  (for-each
    (lambda (s) (exec (sql/transient db s)))
    (list "CREATE TABLE IF NOT EXISTS
             selector (id INTEGER PRIMARY KEY, text TEXT);")))

(defcmd (add-selector text)
  "\"WHERE …\"" "Creates a pre-defined query selector"
  (trace `(add-select ,text))
  (exec (sql db "INSERT INTO selector(text) VALUES (?);") text)
  (write-line (conc " -> " (last-insert-rowid db))))

(define (get-selector id)
  (query fetch-value (sql db "SELECT text FROM selector WHERE id=?;") id))

(defcmd (list-selectors)
  "" "List pre-defined query selectors"
  (query
    (for-each-row
      (lambda (row)
        (write-line (conc "#" (car row) ": \"" (cadr row) "\""))))
    (sql db "SELECT id,text FROM selector;")))

(defcmd (set-selector id text)
  "id \"WHERE …\"" "Sets a pre-defined query selector"
  (trace `(set-selector ,id ,text))
  (exec (sql db "INSERT OR REPLACE INTO selector(id,text) VALUES (?,?);")
        id text))

;;;;;;;;;;;;;;;;;;;;;
;; Database Updates

;; Tag Management

(define (set-tag-auto name auto)
  (exec (sql db "UPDATE tag SET auto=? WHERE name=?;") auto name))

(defcmd (add-auto-tag name . rest)
  "tag-name [tag-name ...]" "Set tags as automatic"
  (trace `(add-auto-tag ,name))
  (set-tag-auto name 1)
  (unless (null? rest)
    (apply add-auto-tag rest)))

(defcmd (add-tag name . rest)
  "tag-name [tag-name ...]" "Create a new tag"
  (trace `(add-tag ,name))
  (exec (sql db "INSERT INTO tag(name) VALUES (?);") name)
  (unless (null? rest)
    (apply add-tag rest)))

(defcmd (auto-tags . tag-list)
  "[tag-name ...]" "Set the list of automatic tags"
  (trace `(auto-tags . ,tag-list))
  (with-transaction db
    (lambda ()
      (exec (sql db "UPDATE tag SET auto=0;"))
      (let loop ((todo tag-list))
        (unless (null? todo)
          (set-tag-auto (car todo) 1)
          (loop (cdr todo)))))))

(defcmd (list-tags)
  "" "List available tag, automatic tags are marked with *"
  (query
    (for-each-row*
      (lambda (name auto count)
        (write-line (conc name (if (zero? auto) " (" "* (") count ")"))))
    (sql db "SELECT name,auto,COUNT(tagrel.url_id)
             FROM tag OUTER LEFT JOIN tagrel ON id=tagrel.tag_id
             GROUP BY id ORDER BY name;")))

(defcmd (remove-auto-tag name . rest)
  "[tag-name ...]" "Set tags as not automatic"
  (trace `(remove-auto-tag ,name))
  (set-tag-auto name 0)
  (unless (null? rest)
    (apply remove-auto-tag rest)))

(defcmd (remove-tag name . rest)
  "tag-name [tag-name ...]" "Remove tags"
  (trace `(remove-tag ,name))
  (exec (sql db "DELETE FROM tag WHERE name=?;") name)
  (unless (null? rest)
    (apply remove-tag rest)))

(defcmd (rename-tag old-name new-name)
  "old-tag-name new-tag-name" "Rename a tag, preserving associations"
  (trace `(rename-tag ,old-name ,new-name))
  (exec (sql db "UPDATE tag SET name=? WHERE name=?;") new-name old-name))

;; Entry Protection

(define (is-protected? entry-id)
  (not (zero?
          (query fetch-value
                 (sql db "SELECT protected FROM entry WHERE id=?;")
                 entry-id))))

(define-syntax unless-protected
  (syntax-rules ()
    ((unless-protected entry-id . form)
      (if (is-protected? entry-id)
          (write-line (conc "Warning: entry " entry-id " is protected"))
          (begin . form)))))

(define (protect! entry-id)
  (trace `(protect! ,entry-id))
  (exec (sql db "UPDATE entry SET protected=? WHERE id=?;") 1 entry-id))

(define (protect* ptime entry-id)
  (trace `(protect ,ptime ,entry-id))
  (unless-protected entry-id
     (exec (sql db "UPDATE entry SET protected=1,ptime=? WHERE id=?;")
           ptime entry-id)))

(defcmd (protect . args)
  "[timestamp] [entry-id]" "Protect entries from modification"
  (cond ((null? args)
          (protect* (current-seconds) cur-entry))
        ((null? (cdr args))
          (protect* (current-seconds) (car args)))
        (else
          (protect* (car args) (cadr args)))))

(define (unprotect! entry-id)
  (trace `(unprotect! ,entry-id))
  (exec (sql db "UPDATE entry SET protected=? WHERE id=?;") 0 entry-id))

(define (unprotect* mtime entry-id)
  (trace `(unprotect ,mtime ,entry-id))
  (exec (sql db "UPDATE entry SET protected=0,ptime=NULL,mtime=? WHERE id=?;")
        mtime entry-id))

(defcmd (unprotect . args)
  "[timestamp] [entry-id]" "Unprotect entries from modification"
  (cond ((null? args)
          (unprotect* (current-seconds) cur-entry))
        ((null? (cdr args))
          (unprotect* (current-seconds) (car args)))
        (else
          (unprotect* (car args) (cadr args)))))

(define (without-protection* entry-id proc)
  (let ((prev-cur-entry-id cur-entry))
    (set! cur-entry entry-id)
    (if (is-protected? entry-id)
        (begin
          (unprotect! entry-id)
          (eval proc)
          (protect! entry-id))
        (eval proc))
    (set! cur-entry prev-cur-entry-id)))

(defcmd (without-protection! first . args)
  "[entry-id] '(...)" "Perform updates bypassing protection"
  (cond ((null? args)
          (without-protection* cur-entry first))
        ((and (null? (cdr args)) (integer? first))
          (without-protection* first (car args)))
        (else (assert #f "Invalid arguments " (cons first args)))))

;; Entry Management

(define cur-entry
  (query fetch-value
         (sql/transient db "SELECT id FROM entry ORDER BY id DESC LIMIT 1;")))

(define (time-id-strings args)
  (cond ((or (null? args) (string? (car args)))
          (list (current-seconds) cur-entry args))
        ((not (integer? (car args)))
          (assert #f "Unknown type parameter for " (car args)))
        ((or (null? (cdr args)) (string? (cadr args)))
          (list (current-seconds) (car args) (cdr args)))
        ((integer? (cadr args))
          (list (car args) (cadr args) (cddr args)))
        (else (assert #f "Unknown type parameter for " (cadr args)))))

(define (add-entry* ctime url notes)
  (trace `(add-entry ,ctime ,url ,notes))
  (let ((new-id
    (with-transaction db
      (lambda ()
        (exec (sql db "INSERT INTO entry(url,notes,ctime,mtime) VALUES (?,?,?,?);")
              url notes ctime ctime)
        (let ((new-id (last-insert-rowid db)))
          (exec (sql db "INSERT INTO tagrel SELECT ?,id FROM tag WHERE auto=1;")
                new-id)
          new-id)))))
    (set! cur-entry new-id)
    (write-line (conc "Added " new-id))))

(defcmd (add-entry first second . rest)
  "[timestamp] URL note-line [note-line ...]" "Create a new entry"
  (if (or (null? rest) (string? first))
      (add-entry* (current-seconds)
                  first
                  (apply string-append (map terminate-line (cons second rest))))
      (add-entry* first
                  second
                  (apply string-append (map terminate-line rest)))))

(define (add-notes* mtime entry-id lines)
  (unless (null? lines)
    (trace `(add-notes ,mtime ,entry-id . ,lines))
    (with-transaction db
      (lambda ()
        (let ((prev-notes (query fetch-value
                                 (sql db "SELECT notes FROM entry WHERE id=?;")
                                 entry-id)))
          (unless-protected entry-id
            (exec (sql db "UPDATE entry SET notes=?,mtime=? WHERE id=?;")
                  (apply string-append prev-notes
                    (map terminate-line lines))
                  mtime
                  entry-id)))))))

(defcmd (add-notes . args)
  "[[timestamp] entry-id] note-line [note-line ...]"
  "Append new lines of notes"
  (apply add-notes* (time-id-strings args)))

(define (print-entry-row id url type descr notes protected ptime ctime mtime tags)
  (write-line (conc vt100-entry-header
                    "#" id (if (zero? protected) "" "*") " - " url
                    vt100-reset))
    (unless (null? ctime) (write-line (conc "Created   " (rfc-3339 ctime))))
    (unless (null? ptime) (write-line (conc "Protected " (rfc-3339 ptime))))
    (unless (null? mtime) (write-line (conc "Modified  " (rfc-3339 mtime))))
    (unless (null? descr)
      (if (null? type)
          (write-line "Descripiton:")
          (write-line (conc "Description (" type "):")))
      (write-string descr))
    (unless (null? notes)
      (write-line (conc "Notes:"))
      (write-string notes))
    (if (null? tags)
        (write-line "No tags.")
        (write-line (string-append "Tags: " tags))))

(define (print-listed-entry-row id url notes protected)
  (write-line (conc vt100-entry-header
                    "#" id (if (zero? protected) "" "*") " - " url
                    vt100-reset))
  (write-string notes))

(defcmd (list-selection str)
  "\"WHERE ...\"" "Display a custom query as an entry list"
  (query (for-each-row* print-listed-entry-row)
         (sql/transient db
           (string-append "SELECT id,url,notes,protected FROM entry "
                          str ";"))))

(defcmd (list-tagged tag-name . args)
  "tag-name [limit]" "Display entries with the given tag"
  (query (for-each-row* print-listed-entry-row)
         (sql db "SELECT * FROM
                    (SELECT id,url,notes,protected FROM entry
                      WHERE id IN (SELECT url_id FROM tagrel
                                    WHERE tag_id IN (SELECT id FROM tag
                                                      WHERE name=?))
                    ORDER BY id DESC LIMIT ?)
                   ORDER BY id ASC;")
         tag-name
         (if (null? args) 10 (car args))))

(defcmd (list-untagged)
  "" "Display entries without any tag"
  (query (for-each-row* print-listed-entry-row)
         (sql db "SELECT id,url,notes,protected FROM entry
                   WHERE id NOT IN (SELECT url_id FROM tagrel);")))

(define (print-entry* entry-id)
  (query (for-each-row* print-entry-row)
         (sql db "SELECT entry.id,url,type,description,notes,
                         protected,ptime,ctime,mtime,group_concat(tag.name,' ')
                  FROM entry
                  LEFT OUTER JOIN tagrel ON entry.id=tagrel.url_id
                  LEFT OUTER JOIN tag ON tag.id=tagrel.tag_id
                  WHERE entry.id=? GROUP BY entry.id;")
         entry-id))

(defcmd (print-entry . args)
  "[entry-id]" "Display an entry"
  (if (null? args)
      (print-entry* cur-entry)
      (let loop ((todo args))
        (unless (null? todo)
          (print-entry* (car todo))
          (loop (cdr todo))))))

(defcmd (print-selection str)
  "\"WHERE ...\"" "Display entries from a custom query"
  (query (for-each-row* print-entry-row)
         (sql/transient db
           (string-append
                 "SELECT entry.id,url,type,description,notes,
                         protected,ptime,ctime,mtime,group_concat(tag.name,' ')
                  FROM entry
                  LEFT OUTER JOIN tagrel ON entry.id=tagrel.url_id
                  LEFT OUTER JOIN tag ON tag.id=tagrel.tag_id "
             str
             " GROUP BY entry.id;"))))

(defcmd (random-tagged tag-name)
  "tag" "Select a random entry with the given tag"
  (let ((entry-id (query fetch-value
                         (sql db "SELECT url_id FROM tagrel WHERE tag_id IN
                                    (SELECT id FROM tag WHERE name=?)
                                  ORDER BY RANDOM() LIMIT 1;")
                         tag-name)))
    (if entry-id
        (begin
          (set! cur-entry entry-id)
          (print-entry))
        (write-line "No such entry found"))))

(defcmd (random-untagged)
  "" "Select a random entry without tag"
  (let ((entry-id (query fetch-value
                         (sql db "SELECT id FROM entry WHERE id NOT IN
                                    (SELECT url_id FROM tagrel)
                                  ORDER BY RANDOM() LIMIT 1;"))))
    (if entry-id
        (begin
          (set! cur-entry entry-id)
          (print-entry))
        (write-line "No such entry found"))))

(define (guess-type str)
  (cond ((null? str) '())
        ((starts-with? "<" str) "html")
        ((or (starts-with? " - " str)
             (starts-with? " + " str)) "markdown-li")
        (else "text")))

(define (set-descr* mtime entry-id type text)
  (trace `(set-descr ,mtime ,entry-id ,type ,text))
  (unless-protected entry-id
    (exec (sql db "UPDATE entry SET type=?,description=?,mtime=? WHERE id=?;")
          type text mtime entry-id)))

(defcmd (set-descr first . args)
  "[[[mtime] entry-id] type] description" "Sets an entry description"
  (case (length args)
    ((0) (set-descr* (current-seconds) cur-entry (guess-type first) first))
    ((1) (set-descr* (current-seconds) cur-entry first (car args)))
    ((2) (set-descr* (current-seconds) first (car args) (cadr args)))
    ((3) (set-descr* first (car args) (cadr args) (caddr args)))
    (else (assert #f "Too many arguments to set-descr " (cons first args)))))

(defcmd (set-entry arg)
  "entry-id|url" "Set current entry"
  (cond ((integer? arg)
          (set! cur-entry arg)
          (when config-verbose (print-entry)))
        ((string? arg)
          (let ((id (query fetch-value
                           (sql db "SELECT id FROM entry WHERE url=?;")
                           arg)))
            (if id
                (begin
                  (set! cur-entry id)
                  (when config-verbose (print-entry)))
                (write-line (conc "No entry found for \"" arg "\"")))))
        (else (assert #f "Unsupported argument type for " arg))))

(define (touch* mtime entry-id)
  (trace `(touch ,mtime ,entry-id))
  (unless-protected entry-id
    (exec (sql db "UPDATE entry SET mtime=? WHERE id=?;") mtime entry-id)))

(define (touch . args)
  (cond ((null? args)
          (touch* (current-seconds) entry-id))
        ((not (integer? (car args)))
          (assert #f "Bad type for " (car args)))
        ((null? (cdr args))
          (touch* (current-seconds) (car args)))
        ((not (integer? (cadr args)))
          (assert #f "Bad type for " (car args)))
        (else
          (touch* (car args) (cadr args)))))

;; Entry Tagging

(define (print-tags* entry-id)
  (write-line (apply conc (append (list "Tags for " entry-id ":")
    (query (map-rows (lambda (x) (string-append " " (car x))))
           (sql db "SELECT tag.name FROM tagrel
                    OUTER LEFT JOIN tag ON tagrel.tag_id=tag.id
                    WHERE url_id=? ORDER BY tag.name;")
           entry-id)))))

(defcmd (print-tags . args)
  "[entry-id ...]" "Print tags associated with an entry"
  (if (null? args)
      (print-tags* cur-entry)
      (let loop ((todo args))
        (unless (null? todo)
          (print-tags* (car todo))
          (loop (cdr todo))))))

(define (exec-on-tags stmt mtime entry-id tag-list)
  (with-transaction db
    (lambda ()
      (unless-protected entry-id
        (let loop ((todo tag-list))
          (if (null? todo)
              (exec (sql db "UPDATE entry SET mtime=? WHERE id=?;")
                    mtime entry-id)
              (let ((tag-id (query fetch-value
                                   (sql db "SELECT id FROM tag WHERE name=?;")
                                   (car todo))))
                (if tag-id
                    (exec stmt entry-id tag-id)
                    (write-line (conc "Unknown tag " (car todo))))
                (loop (cdr todo))))))))
  (print-tags entry-id))

(define (retag* mtime entry-id tag-list)
  (trace `(retag ,mtime ,entry-id . ,tag-list))
  (unless-protected entry-id
    (exec (sql db "DELETE FROM tagrel WHERE url_id=?;") entry-id)
    (exec-on-tags (sql db "INSERT OR IGNORE INTO tagrel VALUES (?,?);")
                  mtime entry-id tag-list)))

(defcmd (retag . args)
  "[[timestamp] entry-id] tag-name [tag-name ...]"
  "Overwrite tag list for an entry"
  (apply retag* (time-id-strings args)))

(define (tag* mtime entry-id tag-list)
  (unless (null? tag-list)
    (trace `(tag ,mtime ,entry-id . ,tag-list))
    (exec-on-tags (sql db "INSERT OR IGNORE INTO tagrel VALUES (?,?);")
                  mtime entry-id tag-list)))

(defcmd (tag . args)
  "[[timestamp] entry-id] tag-name [tag-name ...]"
  "Associate tags to an entry"
  (apply tag* (time-id-strings args)))

(define (untag* mtime entry-id tag-list)
  (unless (null? tag-list)
    (trace `(untag ,mtime ,entry-id . ,tag-list))
    (exec-on-tags (sql db "DELETE FROM tagrel WHERE url_id=? AND tag_id=?;")
                  mtime entry-id tag-list)))

(defcmd (untag . args)
  "[[timestamp] entry-id] tag-name [tag-name ...]"
  "Disssociates tags from an entry"
  (apply untag* (time-id-strings args)))

;;;;;;;;;;;;;;;;;;;;
;; Editor Spawning

(define (edit-descr* entry-id)
  (let ((file-name (create-temporary-file
                     (string-append "."
                       (get-config/default "description-ext" "txt"))))
        (prev-value
           (query fetch-value
                  (sql db "SELECT description FROM entry WHERE id=?;")
                  entry-id)))
    (when (and prev-value (not (null? prev-value)))
      (call-with-output-file file-name
        (lambda (port) (write-string prev-value #f port))))
    (when config-editor
      (process-wait
        (process-run (string-append config-editor " " (qs file-name)))))
    (let ((result (call-with-input-file file-name
                    (lambda (port) (read-string #f port)))))
      (delete-file file-name)
      (if (or (zero? (string-length result))
              (equal? prev-value result))
          #f
          result))))


(defcmd (edit-descr . args)
  "[[mtime] entry-id]" "Describe using an external editor"
  (let ((new-value (case (length args)
                     ((0) (edit-descr* cur-entry))
                     ((1) (edit-descr* (car args)))
                     ((2) (edit-descr* (cadr args)))
                     (else
                       (assert #f "Too many arguments to edit-descr " args)))))
    (when new-value
      (case (length args)
        ((0) (set-descr* (current-seconds)
                         cur-entry
                         (guess-type new-value)
                         new-value))
        ((1) (set-descr* (current-seconds)
                         (car args)
                         (guess-type new-value)
                         new-value))
        ((2) (set-descr* (car args)
                         (cadr args)
                         (guess-type new-value)
                         new-value))
        (else (assert #f "Too many arguments to edit-descr " args))))))

;;;;;;;;;;;;;;;;;;;;
;; Feed Generation

(define activate-feed-stmt
  (sql db "UPDATE feed SET active=? WHERE id=?;"))
(define add-feed-stmt
  (sql db "INSERT INTO feed(filename,url,selector,title) VALUES (?,?,?,?);"))
(define list-active-feed-stmt
  (sql db "SELECT * FROM feed WHERE active=1;"))
(define list-feed-stmt
  (sql db "SELECT * FROM feed;"))
(define remove-feed-stmt
  (sql db "DELETE FROM feed WHERE id=?;"))
(define select-feed-stmt
  (sql db "SELECT * FROM feed WHERE id=?;"))

(defcmd (activate-feed feed-id)
  "feed-id" "Activate the given feed"
  (trace `(activate-feed ,feed-id))
  (exec activate-feed-stmt 1 feed-id))

(defcmd (add-feed filename url selector title)
  "filename url selector title" "Add a new feed"
  (trace `(add-feed ,filename ,url ,selector ,title))
  (exec add-feed-stmt filename url selector title)
  (write-line (conc "Added feed " (last-insert-rowid db))))

(defcmd (disable-feed feed-id)
  "feed-id" "Disable the given feed"
  (trace `(disable-feed ,feed-id))
  (exec activate-feed-stmt 0 feed-id))

(define (atom-content type descr notes)
  (cond ((null? descr) `(atom:content ,notes))
        ((null? type)  `(atom:content ,descr))
        ((equal? type "markdown-li")
          (let ((acc (open-output-string))
                (prev-output (current-output-port)))
            (current-output-port acc)
            (let ((result (markdown->html (substring descr 3))))
              (current-output-port prev-output)
              (if result
                  `(atom:content (@ (type "html")) ,(get-output-string acc))
                  `(atom:content ,descr)))))
        (else `(atom:content (@ (type ,type)) ,descr))))

(define (feed->sxml id url type descr notes ptime ctime mtime)
  `(atom:entry
     (atom:id ,(string-append config-entry-id-prefix (number->string id)))
     (atom:title ,url)
     (atom:updated ,(rfc-3339 mtime))
     (atom:published ,(rfc-3339 (if (null? ptime) ctime ptime)))
     (atom:link (@ (rel "related") (href ,url)))
     ,(atom-content type descr notes)
     ,@(query (map-rows (lambda (x) `(atom:category (@ (term ,(car x))))))
              (sql db "SELECT tag.name FROM tagrel
                       OUTER LEFT JOIN tag ON tagrel.tag_id=tag.id
                       WHERE url_id=? ORDER BY tag.name;")
              id)))

(define (write-feed stmt mtime title self)
  (write-string
    (serialize-sxml
      `(*TOP* (@ (*NAMESPACES* (atom "http://www.w3.org/2005/Atom")))
         (*PI* xml "version='1.0' encoding='utf-8'")
         (atom:feed
           (atom:title ,title)
           (atom:author
             (atom:name ,(if config-author-name
                             config-author-name
                             "Unknown Author"))
             ,@(if config-author-email `((atom:email ,config-author-email)) '())
             ,@(if config-author-uri `((atom:uri ,config-author-uri)) '()))
           (atom:id ,self)
           (atom:link (@ (rel "self") (href ,self)))
           (atom:updated ,(rfc-3339 mtime))
           ,@(map (lambda (row) (apply feed->sxml row))
                  (query fetch-rows stmt))))
      ns-prefixes: '((*default* . "http://www.w3.org/2005/Atom")))))

(define (generate-feed feed-id filename url selector title active-int)
  (let* ((stmt (sql db (string-append "SELECT id,url,type,description,notes,ptime,ctime,mtime FROM entry " selector)))
         (first-row (query fetch-row stmt))
         (mtime (if (null? first-row) #f (list-ref first-row 7))))
    (cond ((not mtime)
            (when config-verbose
              (write-line (conc "Feed " feed-id " is empty"))))
          ((and (file-exists? filename)
                (< mtime (file-modification-time filename)))
            (when config-verbose
              (write-line (conc "Feed " feed-id " is already up to date"))))
          (else
            (when config-verbose
              (write-line (conc "Generating feed " feed-id)))
            (with-output-to-file filename
              (lambda () (write-feed stmt mtime title url)))))))

(defcmd (generate . args)
  "[feed-id ...]" "Generate the given feeds, or all active feeds"
  (let loop ((todo (if (null? args)
                       (query fetch-all list-active-feed-stmt)
                       (map (lambda (id) (query fetch select-feed-stmt id))
                            args))))
    (unless (null? todo)
      (apply generate-feed (car todo))
      (loop (cdr todo)))))

(defcmd (list-feeds)
  "" "Display all feeds"
  (query
    (map-rows*
      (lambda (id filename url selector title active-int)
        (write-line (conc (if (zero? active-int)
                              (conc "(" id ")")
                              (conc "#" id))
                          " "
                          filename
                          " - "
                          title))
        (write-line (conc "    " url))
        (write-line (conc "    " selector))))
    list-feed-stmt))

(defcmd (remove-feed feed-id)
  "feed-id" "Remove the given feed"
  (trace `(remove-feed ,feed-id))
  (exec remove-feed-stmt feed-id))

;;;;;;;;;;;;;
;; Auto Add

(define (auto-add lines)
  (unless arg-replay
    (trace `(auto-add ,lines))
    (let loop ((index 0) (urls '()))
      (let* ((start0 (substring-index-ci "https://" lines index))
             (start  (if start0 start0
                         (substring-index-ci "http://" lines index)))
             (end    (if start
                         (apply min
                           (filter identity
                             (list
                               (string-length lines)
                               (substring-index " " lines start)
                               (substring-index "\n" lines start))))
                         #f)))
        (cond (start
                (loop end (cons (substring lines start end) urls)))
              ((null? urls)
                (write-line (conc "Warning: no URL found")))
              (else
                (for-each (lambda (url) (add-entry url lines)) urls)))))))

;;;;;;;;;;;;;;
;; Main loop

(defcmd (replay filename)
  "filename" "Replay the given file"
  (let ((old-arg-replay arg-replay))
    (set! arg-replay #t)
    (load filename)
    (set! arg-replay old-arg-replay)))

(define (write-query text)
   (query (for-each-row (lambda (row) (if (= 1 (length row))
                                          (write-line (->string (car row)))
                                          (begin (write row) (newline)))))
          (sql/transient db text)))

(defcmd (help)
  "" "Display this help"
  (for-each
    (lambda (row)
      (write-line (conc
        "("
        (car row)
        (if (zero? (string-length (cadr row))) "" " ")
        (cadr row)
        ")"))
      (write-line (conc "    " (caddr row))))
    cmd-list))

(set! cmd-list (sort! cmd-list (lambda (r1 r2) (string<? (car r1) (car r2)))))

(define completion-ptr cmd-list)
(define new-completion #t)
(define (completer prefix state)
  (when (zero? state)
    (set! completion-ptr cmd-list)
    (set! new-completion #t))
  (let ((buf (line-buffer)))
    (cond ((and (positive? (string-length buf))
                (not (eqv? (string-ref buf 0) #\()))
            #f)
          ((substring-index " " buf)
            (let ((other-state (if new-completion 0 state)))
              (set! new-completion #f)
              (scheme-completer prefix other-state)))
          (else
            (let loop ()
              (cond ((null? completion-ptr)
                      #f)
                    ((starts-with? prefix (caar completion-ptr))
                      (let ((result (caar completion-ptr)))
                        (set! completion-ptr (cdr completion-ptr))
                        result))
                    (else
                        (set! completion-ptr (cdr completion-ptr))
                        (loop))))))))

(define state 'general)
(define (prompt)
  (cond ((eqv? state 'general) "> ")
        ((eqv? state 'in-command) "… ")
        (else "? ")))

(define (interactive-main)
  (basic-quote-characters-set! "\"|")
  (completer-word-break-characters-set! "\"\'`;|()[] ")
  (completer-set! completer)
  (variable-bind! "blink-matching-paren" "on")
  (paren-blink-timeout-set! 200000)

  (let ((handler (signal-handler signal/int)))
    (set-signal-handler! signal/int (lambda (s) (cleanup-after-signal!)
                                                (reset-after-signal!)
                                                (handler s))))
  (on-exit reset-terminal!)
  (current-input-port (make-readline-port prompt))

  (let main-loop ()
    (let ((c (peek-char)))
      (cond ((eof-object? c))
            ((eqv? c #\()
              (set! state 'in-command)
              (handle-exceptions
                exn
                (begin
                  (print-error-message exn)
                  (print-call-chain))
                (eval (read)))
              (set! state 'general)
              (main-loop))
            (else
              (let data-loop ((acc (list (read-line))))
                (if (char-ready?)
                    (data-loop (cons (read-line) acc))
                    (let ((lines (reverse-string-append
                                   (map terminate-line acc))))
                      (when (positive? (string-length lines))
                        (auto-add lines))
                      (main-loop)))))))))

(cond ((not arg-replay)
        (interactive-main))
      ((eqv? (string-ref arg-replay 0) #\()
        (eval (read (open-input-string arg-replay))))
      (else
        (load arg-replay)))
