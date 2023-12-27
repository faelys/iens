(import (chicken condition)
        (chicken process signal)
        (chicken process-context)
        (chicken io)
        (chicken string)
        breadline
        sql-de-lite
        srfi-1)

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
    (list "CREATE TABLE config (key TEXT, val TEXT);"
          (conc "INSERT INTO config(key, val) VALUES "
                "('schema-version','" schema-version "');")
          (conc "CREATE TABLE tag (id INTEGER PRIMARY KEY, "
                "name TEXT, auto INTEGER DEFAULT 0);")
          (conc "CREATE TABLE entry (id INTEGER PRIMARY KEY, "
                "url TEXT NOT NULL, type TEXT, description TEXT, notes TEXT, "
                "ctime TEXT DEFAULT CURRENT_TIMESTAMP, "
                "mtime TEXT DEFAULT CURRENT_TIMESTAMP);")
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
(define print-tags-stmt
  (sql db "SELECT name, auto FROM tag;"))
(define remove-tag-stmt
  (sql db "DELETE FROM tag WHERE name = ?;"))
(define rename-tag-stmt
  (sql db "UPDATE tag SET name = ? WHERE name = ?;"))
(define reset-auto-tag-stmt
  (sql db "UPDATE tag SET auto = 0;"))
(define set-auto-tag-stmt
  (sql db "UPDATE tag SET auto = ? WHERE name = ?;"))

(define (add-auto-tag name . rest)
  (trace `(add-auto-tag ,name))
  (exec set-auto-tag-stmt 1 name)
  (unless (null? rest)
    (apply add-auto-tag rest)))

(define (add-tag name . rest)
  (trace `(add-tag ,name))
  (exec add-tag-stmt name)
  (unless (null? rest)
    (apply add-tag rest)))

(define (auto-tags . tag-list)
  (trace `(auto-tags . ,tag-list))
  (with-transaction db
    (lambda ()
      (exec reset-auto-tag-stmt)
      (let loop ((todo tag-list))
        (unless (null? todo)
          (exec set-auto-tag-stmt 1 (car todo))
          (loop (cdr todo)))))))

(define (print-tags)
  (query
    (for-each-row*
      (lambda (name auto)
        (write-line (conc "  " name (if (= 0 auto) "" "*")))))
    print-tags-stmt))

(define (remove-auto-tag name . rest)
  (trace `(remove-auto-tag ,name))
  (exec set-auto-tag-stmt 0 name)
  (unless (null? rest)
    (apply remove-auto-tag rest)))

(define (remove-tag name . rest)
  (trace `(remove-tag ,name))
  (exec remove-tag-stmt name)
  (unless (null? rest)
    (apply remove-tag rest)))

(define (rename-tag old-name new-name)
  (trace `(rename-tag ,old-name ,new-name))
  (exec rename-tag-stmt old-name new-name))

;; Entry Management

(define add-entry-stmt
  (sql db "INSERT INTO entry(url, notes) VALUES (?, ?);"))
(define auto-tag-stmt
  (sql db "INSERT INTO tagrel SELECT ?,id FROM tag WHERE auto = 1;"))
(define select-entry-stmt
  (sql db "SELECT id,url,notes FROM entry WHERE id=?;"))
(define select-untagged-stmt
  (sql db "SELECT id,url,notes FROM entry WHERE id NOT IN (SELECT url_id FROM tagrel);"))
(define set-notes-stmt
  (sql db "UPDATE entry SET notes=? WHERE id=?;"))

(define cur-entry
  (query fetch-value
         (sql/transient db "SELECT id FROM entry ORDER BY id DESC LIMIT 1;")))

(define (add-entry url notes)
  (trace `(add-entry ,url ,notes))
  (let ((new-id
    (with-transaction db
      (lambda ()
        (exec add-entry-stmt url notes)
        (let ((new-id (last-insert-rowid db)))
          (exec auto-tag-stmt new-id)
          new-id)))))
    (set! cur-entry new-id)
    (write-line (conc "Added " new-id))))

(define (add-notes* entry-id lines)
  (unless (null? lines)
    (trace `(add-notes ,entry-id . ,lines))
    (with-transaction db
      (lambda ()
        (let ((prev-notes (caddr (query fetch-row select-entry-stmt entry-id))))
          (exec set-notes-stmt
                (apply string-append prev-notes
                  (map (lambda (s) (string-append s "\n")) lines))
                entry-id))))))

(define (add-notes first . rest)
  (cond ((integer? first) (add-notes* first rest))
        ((string? first)  (add-notes* cur-entry (cons first rest)))
        (else (assert #f "Unknown type parameter for " first))))

(define (print-entry-row row)
  (write-line (conc "#" (car row) " - " (cadr row)))
  (write-string (caddr row)))

(define (print-entry* entry-id)
  (query (for-each-row print-entry-row)
         select-entry-stmt
         entry-id))

(define (print-entry . args)
  (if (null? args)
      (print-entry* cur-entry)
      (let loop ((todo args))
        (unless (null? todo)
          (print-entry* (car todo))
          (loop (cdr todo))))))

(define (print-untagged)
  (query (for-each-row print-entry-row) select-untagged-stmt))

(define (set-entry entry-id)
  (set! cur-entry entry-id))

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

(define (print-tags . args)
  (if (null? args)
      (print-tags* cur-entry)
      (let loop ((todo args))
        (unless (null? todo)
          (print-tags* (car todo))
          (loop (cdr todo))))))

(define (exec-on-tags stmt entry-id tag-list)
  (with-transaction db
    (lambda ()
      (let loop ((todo tag-list))
        (unless (null? todo)
          (let ((tag-id (query fetch-value get-tag-id-stmt (car todo))))
            (if tag-id
                (exec stmt entry-id tag-id)
                (write-line (conc "Unknown tag " (car todo)))))))))
  (print-tags entry-id))

(define (tag* entry-id . tag-list)
  (unless (null? tag-list)
    (trace `(tag ,entry-id . ,tag-list))
    (exec-on-tags include-tag-stmt entry-id tag-list)))

(define (tag first . rest)
  (cond ((integer? first) (apply tag* (cons first rest)))
        ((string? first)  (apply tag* (cons cur-entry (cons first rest))))
        (else (assert #f "Unknown type parameter for " first))))

(define (untag* entry-id . tag-list)
  (unless (null? tag-list)
    (trace `(untag ,entry-id . ,tag-list))
    (exec-on-tags exclude-tag-stmt entry-id tag-list)))

(define (untag first . rest)
  (cond ((integer? first) (apply untag* (cons first rest)))
        ((string? first)  (apply untag* (cons cur-entry (cons first rest))))
        (else (assert #f "Unknown type parameter for " first))))

;;;;;;;;;;;;;
;; Auto Add

(define (auto-add line-list)
  (trace `(auto-add ,line-list)))

;;;;;;;;;;;;;;
;; Main loop

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
                  (let ((lines (apply string-append
                                 (map
                                   (lambda (s)
                                     (if (> (string-length s) 0)
                                         (string-append s "\n")
                                         s))
                                   (reverse acc)))))
                    (when (> (string-length lines) 0)
                      (auto-add lines))
                    (main-loop))))))))
