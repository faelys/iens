(import (chicken condition)
        (chicken process signal)
        (chicken process-context)
        (chicken io)
        (chicken sort)
        (chicken string)
        (chicken time)
        (chicken time posix)
        breadline
        sql-de-lite
        srfi-1)

(define (ends-with? maybe-suffix s)
  (let ((ls  (string-length s))
        (lms (string-length maybe-suffix)))
  (and (>= ls lms)
       (substring=? s maybe-suffix (- ls lms)))))

(define (rfc-3339 seconds)
  (let ((time-str (time->string (seconds->local-time seconds) "%FT%T%z")))
    (assert (= 24 (string-length time-str)))
    (if (equal? "0000" (substring time-str 20))
        (string-append (substring time-str 0 19) "Z")
        (string-append (substring time-str 0 22)
                       ":"
                       (substring time-str 22)))))

(define (terminate-line line)
  (let ((l (string-length line)))
    (if (or (= l 0)
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

;;;;;;;;;;;;;
;; Tracing

; TODO: trace to a file
(define (trace obj)
  (write obj)
  (newline))

;;;;;;;;;;;;;;;;;;;;;;;
;; Persistent Storage

(define db-name
  (let ((arg-list (command-line-arguments)))
    (if (>= (length arg-list) 1)
        (car arg-list)
        "iens.sqlite")))
(define db
  (open-database db-name))
(write-line (conc "Using database " db-name " with SQLite " library-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database Creation/Migration

(define schema-version 1)

(when (null? (schema db))
  (write-line (conc "Initializing database with schema v" schema-version))
  (for-each
    (lambda (s) (exec (sql db s)))
    (list "CREATE TABLE config (key TEXT, val);"
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
          "CREATE UNIQUE INDEX i_url ON entry(url);"
          "CREATE UNIQUE INDEX i_tag ON tag(name);"
          "CREATE UNIQUE INDEX i_rel ON tagrel(url_id,tag_id);")))

;;;;;;;;;;;;;;;;;;;;;
;; Database Updates

;; Tag Management

(define add-tag-stmt
  (sql db "INSERT INTO tag(name) VALUES (?);"))
(define list-tags-stmt
  (sql db "SELECT name, auto FROM tag;"))
(define remove-tag-stmt
  (sql db "DELETE FROM tag WHERE name = ?;"))
(define rename-tag-stmt
  (sql db "UPDATE tag SET name = ? WHERE name = ?;"))
(define reset-auto-tag-stmt
  (sql db "UPDATE tag SET auto = 0;"))
(define set-auto-tag-stmt
  (sql db "UPDATE tag SET auto = ? WHERE name = ?;"))

(defcmd (add-auto-tag name . rest)
  "tag-name [tag-name ...]" "Set tags as automatic"
  (trace `(add-auto-tag ,name))
  (exec set-auto-tag-stmt 1 name)
  (unless (null? rest)
    (apply add-auto-tag rest)))

(defcmd (add-tag name . rest)
  "tag-name [tag-name ...]" "Create a new tag"
  (trace `(add-tag ,name))
  (exec add-tag-stmt name)
  (unless (null? rest)
    (apply add-tag rest)))

(defcmd (auto-tags . tag-list)
  "[tag-name ...]" "Set the list of automatic tags"
  (trace `(auto-tags . ,tag-list))
  (with-transaction db
    (lambda ()
      (exec reset-auto-tag-stmt)
      (let loop ((todo tag-list))
        (unless (null? todo)
          (exec set-auto-tag-stmt 1 (car todo))
          (loop (cdr todo)))))))

(defcmd (list-tags)
  "" "List available tag, automatic tags are marked with *"
  (query
    (for-each-row*
      (lambda (name auto)
        (write-line (conc "  " name (if (= 0 auto) "" "*")))))
    list-tags-stmt))

(defcmd (remove-auto-tag name . rest)
  "[tag-name ...]" "Set tags as not automatic"
  (trace `(remove-auto-tag ,name))
  (exec set-auto-tag-stmt 0 name)
  (unless (null? rest)
    (apply remove-auto-tag rest)))

(defcmd (remove-tag name . rest)
  "tag-name [tag-name ...]" "Remove tags"
  (trace `(remove-tag ,name))
  (exec remove-tag-stmt name)
  (unless (null? rest)
    (apply remove-tag rest)))

(defcmd (rename-tag old-name new-name)
  "old-tag-name new-tag-name" "Rename a tag, preserving associations"
  (trace `(rename-tag ,old-name ,new-name))
  (exec rename-tag-stmt old-name new-name))

;; Entry Protection

(define get-protected-stmt
  (sql db "SELECT protected FROM entry WHERE id=?;"))
(define set-protected-stmt
  (sql db "UPDATE entry SET protected=1,ptime=? WHERE id=?;"))
(define tmp-protected-stmt
  (sql db "UPDATE entry SET protected=? WHERE id=?;"))
(define unset-protected-stmt
  (sql db "UPDATE entry SET protected=0,ptime=NULL,mtime=? WHERE id=?;"))

(define (is-protected? entry-id)
  (not (= 0 (query fetch-value get-protected-stmt entry-id))))

(define-syntax unless-protected
  (syntax-rules ()
    ((unless-protected entry-id . form)
      (if (is-protected? entry-id)
          (write-line (conc "Warning: entry " entry-id " is protected"))
          (begin . form)))))

(define (protect! time entry-id)
  (trace `(protect! ,time ,entry-id))
  (exec tmp-protected-stmt 1 entry-id))

(define (protect* ptime entry-id)
  (trace `(protect ,ptime ,entry-id))
  (unless-protected entry-id
     (exec set-protected-stmt ptime entry-id)))

(defcmd (protect . args)
  "[timestamp] [entry-id]" "Protect entries from modification"
  (cond ((null? args)
          (protect* (current-seconds) cur-entry))
        ((null? (cdr args))
          (protect* (current-seconds) (car args)))
        (else
          (protect* (car args) (cadr args)))))

(define (unprotect! time entry-id)
  (trace `(unprotect! ,time ,entry-id))
  (exec tmp-protected-stmt 0 entry-id))

(define (unprotect* mtime entry-id)
  (trace `(unprotect ,mtime ,entry-id))
  (exec unset-protected-stmt mtime entry-id))

(defcmd (unprotect . args)
  "[timestamp] [entry-id]" "Unprotect entries from modification"
  (cond ((null? args)
          (unprotect* (current-seconds) cur-entry))
        ((null? (cdr args))
          (unprotect* (current-seconds) (car args)))
        (else
          (unprotect* (car args) (cadr args)))))

(define (without-protection* time entry-id proc)
  (if (is-protected? entry-id)
      (begin
        (unprotect! time entry-id)
        (eval proc)
        (protect! time entry-id))
      (eval proc)))

(defcmd (without-protection! first . args)
  "[[timestamp] entry-id] '(...)" "Perform updates bypassing protection"
  (cond ((null? args)
          (without-protection* (current-seconds) cur-entry first))
        ((and (null? (cdr args)) (integer? first))
          (without-protection* (current-seconds) first (car args)))
        ((and (null? (cddr args)) (integer? first) (integer? (car args)))
          (without-protection* first (car args) (cadr args)))
        (else (assert #f "Invalid arguments " (cons first args)))))

;; Entry Management

(define add-entry-stmt
  (sql db "INSERT INTO entry(url, notes, ctime, mtime) VALUES (?, ?, ?, ?);"))
(define auto-tag-stmt
  (sql db "INSERT INTO tagrel SELECT ?,id FROM tag WHERE auto = 1;"))
(define list-untagged-stmt
  (sql db "SELECT id,url,notes FROM entry WHERE id NOT IN (SELECT url_id FROM tagrel);"))
(define select-entry-stmt
  (sql db "SELECT id,url,type,description,notes,protected,ptime,ctime,mtime FROM entry WHERE id=?;"))
(define set-notes-stmt
  (sql db "UPDATE entry SET notes=?,mtime=? WHERE id=?;"))
(define touch-entry-stmt
  (sql db "UPDATE entry SET mtime=? WHERE id=?;"))

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
        (exec add-entry-stmt url notes ctime ctime)
        (let ((new-id (last-insert-rowid db)))
          (exec auto-tag-stmt new-id)
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
        (let ((prev-notes (caddr (query fetch-row select-entry-stmt entry-id))))
          (unless-protected entry-id
            (exec set-notes-stmt
                  (apply string-append prev-notes
                    (map terminate-line lines))
                  mtime
                  entry-id)))))))

(defcmd (add-notes . args)
  "[[timestamp] entry-id] note-line [note-line ...]"
  "Append new lines of notes"
  (apply add-notes* (time-id-strings args)))

(define (print-entry-row row)
  (let ((id         (list-ref row 0))
        (url        (list-ref row 1))
        (type       (list-ref row 2))
        (descr      (list-ref row 3))
        (notes      (list-ref row 4))
        (protected? (not (= 0 (list-ref row 5))))
        (ptime      (list-ref row 6))
        (ctime      (list-ref row 7))
        (mtime      (list-ref row 8)))
    (write-line (conc "#" id (if protected? "*" "") " - " url))
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
      (write-string notes))))

(define (print-listed-entry-row row)
  (write-line (conc "#" (car row) " - " (cadr row)))
  (write-string (caddr row)))

(define (print-entry* entry-id)
  (query (for-each-row print-entry-row)
         select-entry-stmt
         entry-id))

(defcmd (print-entry . args)
  "[entry-id]" "Display an entry"
  (if (null? args)
      (print-entry* cur-entry)
      (let loop ((todo args))
        (unless (null? todo)
          (print-entry* (car todo))
          (loop (cdr todo))))))

(defcmd (list-untagged)
  "" "Display entries without any tag"
  (query (for-each-row print-listed-entry-row) list-untagged-stmt))

(defcmd (set-entry entry-id)
  "entry-id" "Set current entry"
  (assert (integer? entry-id))
  (set! cur-entry entry-id))

(define (touch* mtime entry-id)
  (trace `(touch ,mtime ,entry-id))
  (unless-protected entry-id
    (exec touch-entry-stmt mtime entry-id)))

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

(define exclude-tag-stmt
  (sql db "DELETE FROM tagrel WHERE url_id=? AND tag_id=?;"))
(define include-tag-stmt
  (sql db "INSERT OR IGNORE INTO tagrel VALUES (?, ?);"))
(define select-tags-stmt
  (sql db "SELECT tag.name FROM tagrel OUTER LEFT JOIN tag ON tagrel.tag_id = tag.id WHERE url_id=?;"))
(define get-tag-id-stmt
  (sql db "SELECT id FROM tag WHERE name = ?;"))

(define (print-tags* entry-id)
  (write-line (apply conc (append (list "Tags for " entry-id ":")
    (query (map-rows (lambda (x) (string-append " " (car x))))
           select-tags-stmt entry-id)))))

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
      (let loop ((todo tag-list))
        (if (null? todo)
            (exec touch-entry-stmt mtime entry-id)
            (let ((tag-id (query fetch-value get-tag-id-stmt (car todo))))
              (if tag-id
                  (unless-protected entry-id
                    (exec stmt entry-id tag-id))
                  (write-line (conc "Unknown tag " (car todo)))))))))
  (print-tags entry-id))

(define (tag* mtime entry-id tag-list)
  (unless (null? tag-list)
    (trace `(tag ,mtime ,entry-id . ,tag-list))
    (exec-on-tags include-tag-stmt mtime entry-id tag-list)))

(defcmd (tag . args)
  "[[timestamp] entry-id] tag-name [tag-name ...]"
  "Associate tags to an entry"
  (apply tag* (time-id-strings args)))

(define (untag* mtime entry-id tag-list)
  (unless (null? tag-list)
    (trace `(untag ,mtime ,entry-id . ,tag-list))
    (exec-on-tags exclude-tag-stmt mtime entry-id tag-list)))

(defcmd (untag . args)
  "[[timestamp] entry-id] tag-name [tag-name ...]"
  "Disssociates tags from an entry"
  (apply untag* (time-id-strings args)))

;;;;;;;;;;;;;
;; Auto Add

(define (auto-add lines)
  (trace `(auto-add ,lines))
  (let loop ((index 0) (urls '()))
    (let* ((start0 (substring-index-ci "https://" lines index))
           (start  (if start0 start0
                       (substring-index-ci "http://" lines index)))
           (end    (if start
                       (min (string-length lines)
                            (substring-index " " lines start)
                            (substring-index "\n" lines start))
                       #f)))
      (cond (start
              (loop end (cons (substring lines start end) urls)))
            ((null? urls)
              (write-line (conc "Warning: no URL found")))
            (else
              (for-each (lambda (url) (add-entry url lines)) urls))))))

;;;;;;;;;;;;;;
;; Main loop

(defcmd (help)
  "" "Display this help"
  (for-each
    (lambda (row)
      (write-line (conc
        "("
        (car row)
        (if (> (string-length (cadr row)) 0) " " "")
        (cadr row)
        ")"))
      (write-line (conc "    " (caddr row))))
    cmd-list))

(sort! cmd-list (lambda (r1 r2) (string<? (car r1) (car r2))))

(set-signal-handler! signal/int
                     (lambda _
                       (cleanup-after-signal!)
                       (reset-after-signal!)))
(on-exit reset-terminal!)

(define state 'general)
(define (prompt)
  (cond ((eqv? state 'general) "> ")
        ((eqv? state 'in-command) "... ")
        (else "??? ")))
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
                    (when (> (string-length lines) 0)
                      (auto-add lines))
                    (main-loop))))))))
