; Copyright (c) 2023-2026, Natacha Porté
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
        http-client
        lowdown
        ncurses
        rss
        sql-de-lite
        srfi-1
        sxml-serializer)

(define (starts-with? maybe-prefix s)
  (and (<= (string-length maybe-prefix) (string-length s))
       (substring=? s maybe-prefix 0 0 (string-length maybe-prefix))))

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

(define vt100-alert        "\033[31m")
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

(include "common.scm")

(assert (= 5 (db-version)))

;;;;;;;;;;;;;;;;;;
;; Configuration

(define config-author-name #f)
(define config-author-email #f)
(define config-author-uri #f)
(define config-autogenerate #f)
(define config-editor #f)
(define config-entry-id-prefix "")
(define config-list-tagged-count 0)
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
  (set! config-autogenerate (not (zero? (get-config/default "autogenerate" 0))))
  (set! config-editor       (get-config/default "editor" default-editor))
  (set! config-entry-id-prefix (get-config/default "entry-id-prefix" ""))
  (set! config-list-tagged-count (get-config/default "list-tagged-count" 0))
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

(defcmd (add-selector text)
  "\"WHERE …\"" "Creates a pre-defined query selector"
  (trace `(add-select ,text))
  (exec (sql db "INSERT INTO selector(text) VALUES (?);") text)
  (write-line (conc " -> " (last-insert-rowid db))))

(define (call-with-selector arg proc)
  (cond ((string? arg) (proc arg #f))
        ((number? arg) (let ((selector (get-selector arg)))
                         (if selector
                             (proc selector arg)
                             (write-line
                               (conc "No selector #" arg " found")))))
        (else (write-line (conc "Invalid selection argument " arg)))))

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

;; Feed Management

(define (set-feed-active id n)
  (exec (sql db "UPDATE feed SET active=? WHERE id=?;") n id))

(defcmd (activate-feed feed-id)
  "feed-id" "Activate the given feed"
  (trace `(activate-feed ,feed-id))
  (set-feed-active feed-id 1))

(defcmd (add-feed filename url selector title)
  "filename url selector title" "Add a new feed"
  (trace `(add-feed ,filename ,url ,selector ,title))
  (exec (sql db
             "INSERT INTO feed(filename,url,selector,title) VALUES (?,?,?,?);")
        filename url selector title)
  (write-line (conc "Added feed " (last-insert-rowid db))))

(defcmd (disable-feed feed-id)
  "feed-id" "Disable the given feed"
  (trace `(disable-feed ,feed-id))
  (set-feed-active feed-id 0))

(defcmd (list-feeds)
  "" "Display all feeds"
  (query
    (map-rows*
      (lambda (id filename url selector title active-int mtime)
        (write-line (conc (if (zero? active-int)
                              (conc "(" id ")")
                              (conc "#" id))
                          " "
                          filename
                          " - "
                          title))
        (write-line (conc "    " url))
        (write-line (conc "    " selector))
        (unless (null? mtime)
          (write-line (conc "    Updated " (rfc-3339 mtime))))))
    (sql db "SELECT id,filename,url,selector,title,active,mtime FROM feed;")))

(defcmd (remove-feed feed-id)
  "feed-id" "Remove the given feed"
  (trace `(remove-feed ,feed-id))
  (exec (sql db "DELETE FROM feed WHERE id=?;") feed-id))

(define (touch-feed mtime feed-id)
  (trace `(touch-feed ,mtime ,feed-id))
  (exec (sql db "UPDATE feed SET mtime=? WHERE id=?;") mtime feed-id))

;; Feed Caching

(define (build-signature selector)
  (query fetch-rows
         (sql db (string-append "SELECT id,mtime FROM entry " selector ";"))))

(define (car< a b) (< (car a) (car b)))

(define (diff-signature old-sig new-sig)
  (let loop ((old    (sort old-sig car<))
             (new    (sort new-sig car<))
             (result '()))
    (cond ((and (null? old) (null? new))
              result)
          ((null? old)
              (loop old
                    (cdr new)
                    (cons `(add ,@(car new)) result)))
          ((null? new)
              (loop (cdr old)
                    new
                    (cons `(del ,@(car old)) result)))
          ((equal? (car new) (car old))
              (loop (cdr old)
                    (cdr new)
                    result))
          ((= (caar new) (caar old))
              (loop (cdr old)
                    (cdr new)
                    (cons `(chg ,@(car old) ,(cadar new)) result)))
          ((< (caar new) (caar old))
              (loop old
                    (cdr new)
                    (cons `(add ,@(car new)) result)))
          ((> (caar new) (caar old))
              (loop (cdr old)
                    new
                    (cons `(del ,@(car old)) result)))
          (else (assert #f "Should be unreachable")))))

(define (write-diff sig-diff)
  (for-each
    (lambda (hunk)
      (cond ((eqv? (car hunk) 'add)
              (write-line (conc "    added item #" (cadr hunk)
                                " at " (rfc-3339 (caddr hunk)))))
            ((eqv? (car hunk) 'del)
              (write-line (conc "    removed item #" (cadr hunk)
                                " at " (rfc-3339 (caddr hunk)))))
            ((eqv? (car hunk) 'chg)
              (write-line (conc "    updated item #" (cadr hunk)
                                ": " (rfc-3339 (caddr hunk))
                                " → " (rfc-3339 (cadddr hunk)))))
          (else (assert #f "Should be unreachable"))))
    sig-diff))

(define feed-cache
  (query (map-rows* (lambda (id selector)
                            (cons id (build-signature selector))))
         (sql db "SELECT id,selector FROM feed WHERE active=1;")))
(define dirty-feeds '())

(define (check-feed* id)
  (let ((new (query fetch-value
                    (sql db "SELECT selector FROM feed WHERE id=?;")
                    id))
        (old (alist-ref id feed-cache = '())))
    (cond ((and (not new) (null? old))
            (write-line (conc "Feed #" id " does not exist")))
          ((not new)
            (write-line (conc "Feed #" id " does not exist anymore")))
          ((null? old)
            (write-line (conc "Feed #" id " is not cached")))
          (else
            (let ((sig-diff (diff-signature old (build-signature new))))
              (if (null? sig-diff)
                  (write-line (conc "Feed #" id " has not changed"))
                  (write-line (conc "Feed #" id " was modified:")))
              (write-diff sig-diff))))))

(defcmd (check-feed . args)
  "[feed-id ...]" "Check the cache for the given feeds, or all active feeds"
  (for-each check-feed*
    (if (null? args)
        (query fetch-column (sql db "SELECT id FROM feed WHERE active=1;"))
        args)))

(define (update-feed-cache* mtime id)
  (let ((data (query fetch-row
                     (sql db "SELECT mtime,selector,filename,title,url
                              FROM feed WHERE id=?;")
                     id))
        (old-sig (alist-ref id feed-cache = '())))
    (if (null? data)
        (write-line (conc "Feed #" id " does not exist"))
        (let ((new-sig (build-signature (cadr data))))
          (unless (equal? old-sig new-sig)
            (when (or (null? (car data))
                      (> mtime (car data)))
              (touch-feed mtime id)
              (set! (car data) mtime))
            (when config-verbose
               (write-line (if config-autogenerate
                               (conc "Autogenerating feed " id)
                               (conc "Marking feed " id " as dirty:")))
               (write-diff (diff-signature old-sig new-sig)))
            (if config-autogenerate
                (with-output-to-file (caddr data) ;filename
                  (cut write-feed
                    (car data) ;mtime
                    (list-ref data 3) ;title
                    (list-ref data 4) ;url
                    (query fetch-rows
                      (sql db (string-append "SELECT id,url,type,description,
                                                     notes,ptime,ctime,mtime
                                              FROM entry " (cadr data) ";")))))
                (unless (any (cut = id <>) dirty-feeds)
                  (set! dirty-feeds (cons id dirty-feeds))))
            (set! feed-cache
                  (alist-update! id new-sig feed-cache =)))))))

(define (update-feed-cache mtime . id-list)
  (for-each
    (cut update-feed-cache* mtime <>)
    (if (null? id-list)
        (query fetch-column (sql db "SELECT id FROM feed WHERE active=1;"))
        id-list)))

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

(define (n-split l n)
  (let loop ((todo-l l) (todo-n n) (acc '()))
    (if (or (zero? todo-n) (null? todo-l))
        (reverse acc)
        (let ((chunk-size (ceiling (/ (length todo-l) todo-n))))
          (loop (drop todo-l chunk-size)
                (sub1 todo-n)
                (cons (take todo-l chunk-size) acc))))))

(define (expand-cols cols)
  (let loop ((todo cols) (acc '()))
    (if (> (length todo) 1)
        (loop
          (cons (append (cadr todo)
                        (make-list (- (length (car todo)) (length (cadr todo)))
                                   ""))
                (cddr todo))
          (let ((width (apply max (map string-length (car todo)))))
            (cons
              (append
                (map (lambda (s t)
                       (string-append
                         s
                         (make-string (- width -2 (string-length s))
                                      #\space)))
                     (car todo)
                     (cadr todo))
                (drop (car todo) (length (cadr todo))))
              acc)))
        (reverse (append todo acc)))))

(defcmd (list-tags #!optional (cols 1) (threshold 0))
  "[n-columns [min-count]]"
  "List available tag, automatic tags are marked with *"
  (apply for-each
         (lambda row
           (write-line (apply string-append row)))
         (expand-cols
           (n-split
             (query
               (map-rows*
                 (lambda (name auto count)
                   (conc name (if (zero? auto) " (" "* (") count ")")))
               (sql db "SELECT name,auto,COUNT(tagrel.url_id) AS cnt
                        FROM tag OUTER LEFT JOIN tagrel ON id=tagrel.tag_id
                        GROUP BY id HAVING cnt >= ? ORDER BY name;")
               threshold)
             cols))))

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

(define protection-overrides '())

(define (is-overridden? entry-id)
  (any (cut = entry-id <>) protection-overrides))

(define (update-allowed? entry-id)
  (or (not (is-protected? entry-id)) (is-overridden? entry-id)))

(define-syntax unless-protected
  (syntax-rules ()
    ((unless-protected entry-id . form)
      (if (update-allowed? entry-id)
          (begin . form)
          (write-line (conc "Warning: entry " entry-id " is protected"))))))

(define (unoverride! entry-id)
  (trace `(unoverride! ,entry-id))
  (set! protection-overrides (delete! entry-id protection-overrides =)))

(define (protect* ptime entry-id)
  (trace `(protect ,ptime ,entry-id))
  (unless-protected entry-id
     (exec (sql db "UPDATE entry SET protected=1,ptime=? WHERE id=?;")
           ptime entry-id)
     (update-feed-cache ptime)))

(defcmd (protect . args)
  "[[timestamp] entry-id]" "Protect entries from modification"
  (cond ((null? args)
          (protect* (current-seconds) cur-entry))
        ((null? (cdr args))
          (protect* (current-seconds) (car args)))
        (else
          (protect* (car args) (cadr args)))))

(define (override! entry-id)
  (trace `(override! ,entry-id))
  (unless (update-allowed? entry-id)
    (set! protection-overrides (cons entry-id protection-overrides))))

(define (unprotect* mtime entry-id)
  (trace `(unprotect ,mtime ,entry-id))
  (exec (sql db "UPDATE entry SET protected=0,ptime=NULL,mtime=? WHERE id=?;")
        mtime entry-id)
  (update-feed-cache mtime))

(defcmd (unprotect . args)
  "[[timestamp] entry-id]" "Unprotect entries from modification"
  (cond ((null? args)
          (unprotect* (current-seconds) cur-entry))
        ((null? (cdr args))
          (unprotect* (current-seconds) (car args)))
        (else
          (unprotect* (car args) (cadr args)))))

(define (without-protection* entry-id proc)
  (if (or (procedure? proc) (list? proc))
      (let ((prev-cur-entry-id cur-entry))
        (set! cur-entry entry-id)
        (if (is-protected? entry-id)
            (begin
              (override! entry-id)
              (if (procedure? proc) (proc) (eval proc))
              (unoverride! entry-id))
            (if (procedure? proc) (proc) (eval proc)))
        (set! cur-entry prev-cur-entry-id))
      (write-line (conc "Invalid procedure " proc))))

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
    (write-line (conc "Added " new-id)))
  (update-feed-cache ctime))

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
                  entry-id))))))
  (update-feed-cache mtime))

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

(define (count-selection* text id)
  (write-line (string-append (if id (conc "#" id ": ") "")
                             "\"" text "\""))
  (write-line (conc " -> " (query fetch-value
                                  ((if id sql sql/transient)
                                    db
                                    (string-append
                                      "SELECT COUNT(id) FROM entry "
                                      text ";"))))))

(defcmd (count-selection . args)
  "\"WHERE ...\"|selector-id ..." "Count results of a custom queries"
  (if (null? args)
      (query (for-each-row* count-selection*)
             (sql db "SELECT text,id FROM selector;"))
      (let loop ((todo args))
        (unless (null? todo)
          (call-with-selector (car todo) count-selection*)
          (loop (cdr todo))))))

(defcmd (list-selection arg)
  "\"WHERE ...\"|selector-id" "Display a custom query as an entry list"
  (call-with-selector arg
    (lambda (selector id)
      (query (for-each-row* print-listed-entry-row)
             ((if id sql sql/transient) db
               (string-append "SELECT id,url,notes,protected FROM entry "
                              selector ";"))))))

(defcmd (list-tagged tag-name #!optional (count config-list-tagged-count))
  "tag-name [limit]" "Display entries with the given tag"
  (query (for-each-row* print-listed-entry-row)
         (sql db (cond ((positive? count)
                         "SELECT * FROM
                            (SELECT id,url,notes,protected FROM entry
                              WHERE id IN (SELECT url_id FROM tagrel
                                            WHERE tag_id IN (SELECT id FROM tag
                                                              WHERE name=?))
                            ORDER BY id DESC LIMIT ?)
                           ORDER BY id ASC;")
                       ((negative? count)
                         "SELECT id,url,notes,protected FROM entry
                            WHERE id IN (SELECT url_id FROM tagrel
                                          WHERE tag_id IN (SELECT id FROM tag
                                                            WHERE name=?))
                          ORDER BY id ASC LIMIT ?;")
                       (else ; (zero? count)
                         "SELECT id,url,notes,protected FROM entry
                            WHERE id IN (SELECT url_id FROM tagrel
                                          WHERE tag_id IN (SELECT id FROM tag
                                                            WHERE name=?))
                              OR id=?
                          ORDER BY id ASC;")))
         tag-name
         (abs count)))

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

(defcmd (print-selection arg)
  "\"WHERE ...\"|selector-id" "Display entries from a custom query"
  (call-with-selector arg
    (lambda (selector id)
      (query
        (for-each-row* print-entry-row)
        ((if id sql sql/transient) db
          (string-append
            "SELECT entry.id,url,type,description,notes,
                    protected,ptime,ctime,mtime,group_concat(tag.name,' ')
             FROM entry
             LEFT OUTER JOIN tagrel ON entry.id=tagrel.url_id
             LEFT OUTER JOIN tag ON tag.id=tagrel.tag_id "
            selector
            " GROUP BY entry.id;"))))))

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
          type text mtime entry-id)
    (update-feed-cache mtime)))

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
    (exec (sql db "UPDATE entry SET mtime=? WHERE id=?;") mtime entry-id)
    (update-feed-cache mtime)))

(define (touch . args)
  (cond ((null? args)
          (touch* (current-seconds) cur-entry))
        ((not (integer? (car args)))
          (assert #f "Bad type for " (car args)))
        ((null? (cdr args))
          (touch* (current-seconds) (car args)))
        ((not (integer? (cadr args)))
          (assert #f "Bad type for " (car args)))
        (else
          (touch* (car args) (cadr args)))))

(define (without-mtime* entry-id proc)
  (if (or (procedure? proc) (list? proc))
      (let ((prev-entry cur-entry)
            (prev-mtime (query fetch-value
                               (sql db "SELECT mtime FROM entry WHERE id=?;")
                               entry-id)))
        (set! cur-entry entry-id)
        (if (procedure? proc) (proc) (eval proc))
        (touch* prev-mtime entry-id)
        (set! cur-entry prev-entry))
      (write-line (conc "Invalid procedure " proc))))

(defcmd (without-mtime! first . args)
  "[entry-id] '(...)" "Perform updates and restore entry mtime"
  (cond ((null? args)
          (without-mtime* cur-entry first))
        ((and (null? (cdr args)) (integer? first))
          (without-mtime* first (car args)))
        (else (assert #f "Invalid arguments " (cons first args)))))

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


(define (resolve-tag-id tag-name)
  (let ((result (query fetch-value
                       (sql db "SELECT id from tag WHERE name=?;")
                       tag-name)))
    (unless result
      (write-line (conc "Unknown tag " tag-name)))
    result))

(define (exec-on-tags stmt mtime entry-id tag-list)
  (with-transaction db
    (lambda ()
      (unless-protected entry-id
        (let ((tag-id-list (map resolve-tag-id tag-list)))
          (when (every identity tag-id-list)
            (let loop ((todo tag-id-list))
              (if (null? todo)
                  (exec (sql db "UPDATE entry SET mtime=? WHERE id=?;")
                        mtime entry-id)
                  (begin
                    (exec stmt entry-id (car todo))
                    (loop (cdr todo))))))))))
  (print-tags entry-id)
  (update-feed-cache mtime))

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

(define (edit-descr* entry-id)
  (let ((file-name (create-temporary-file
                     (string-append "."
                       (get-config/default "description-ext" "txt"))))
        (fields
           (query fetch-row
                  (sql db "SELECT description,notes,url FROM entry WHERE id=?;")
                  entry-id)))
    (unless (null? fields)
      (call-with-output-file file-name
        (lambda (port)
          (if (or (null? (car fields)) (string=? (car fields) ""))
            (let* ((s-sec (substring-index "[" (cadr fields)))
                   (e-sec (if s-sec
                              (substring-index "]" (cadr fields) s-sec)
                              #f))
                   (sect  (if e-sec
                              (substring (cadr fields) (+ s-sec 1) e-sec) #f))
                   (comm  (if sect (comment-link sect (caddr fields)) #f)))
              (write-string (conc " + [](" (caddr fields) ")\n") #f port)
              (when sect
                (write-string
                  (conc "(via "
                    (if comm (conc "[" sect "](" comm ")") sect)
                    " sur #gcufeed)\n")
                  #f port)))
            (write-string (car fields) #f port))
          (unless (null? (cadr fields))
            (write-string "-+-+-\n" #f port)
            (write-string (cadr fields) #f port)))))
    (when config-editor
      (process-wait
        (process-run (string-append config-editor " " (qs file-name)))))
    (let ((result (call-with-input-file file-name
                    (lambda (port)
                      (let* ((text (read-string #f port))
                             (end  (substring-index-ci "-+-+-\n" text)))
                        (if end
                            (substring text 0 end)
                            text))))))
      (delete-file file-name)
      (if (or (zero? (string-length result))
              (equal? (if (or (null? fields) (null? (car fields)))
                          "" (car fields))
                      result))
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

(define (auto-cols widths avail)
  (letrec ((len     (vector-length widths))
         (w-slice (lambda (start len acc)
                    (if (< len 1)
                        acc
                        (w-slice (+ start 1) (- len 1)
                                 (max acc (vector-ref widths start))))))
         (w-total (lambda (start stride acc)
                    (if (< (+ start stride) len)
                        (w-total (+ start stride)
                                 stride
                                 (cons (+ (car acc) 1 (w-slice start stride 0))
                                       acc))
                        (cons (+ (car acc) (w-slice start (- len start) 0))
                              acc))))
         (h-cols  (lambda (ncols) (quotient (+ len ncols -1) ncols)))
         (w-cols  (lambda (ncols) (w-total 0 (h-cols ncols) (list 0)))))
    (let loop ((ncols len) (best #f))
      (if (zero? ncols) best
        (let ((w (w-cols ncols)) (h (h-cols ncols)))
          (loop (- ncols 1)
                (if (and (< (car w) avail)
                         (or (not best) (<= h (car best))))
                    (list h (list->vector (reverse (cdr w))))
                    best)))))))

(define (select-tags** entry-id tags)
  (let* ((ntags (vector-length tags))
         (state (list->vector (map cadddr (vector->list tags))))
         (cols (auto-cols (list->vector
                            (map (lambda (x)
                                   (+ (string-length (cadr x))
                                      (string-length (caddr x))))
                                 (vector->list tags))) (COLS)))
         (stride (car cols))
         (x-cols (cadr cols))
         (show-tag (lambda (index sel)
                     (unless (zero? (vector-ref state index))
;                      (attron (COLOR_PAIR 1)))
                       (attron A_REVERSE))
                     (when (= index sel)
;                      (attron A_REVERSE))
                       (attron A_UNDERLINE))
                     (mvprintw
                       (remainder index stride)
                       (vector-ref x-cols (quotient index stride))
                       "~A~A"
                       (cadr (vector-ref tags index))
                       (caddr (vector-ref tags index)))
                     (when (= index sel)
;                      (attroff A_REVERSE))
                       (attroff A_UNDERLINE))
                     (unless (zero? (vector-ref state index))
;                      (attroff (COLOR_PAIR 1)))))
                       (attroff A_REVERSE))))
         (update-tags (lambda (old new) (show-tag old new) (show-tag new new))))
    (keypad (stdscr) #t)
    (noecho)
    (curs_set 0)
;   (start_color)
;   (init_pair 1 COLOR_BLUE COLOR_BLACK)
    (let init ((index 0))
      (when (< index ntags)
        (show-tag index 0)
        (init (+ index 1))))
    (let loop ((sel 0))
      (let ((c (char->integer (getch))))
        (cond
          ((= c KEY_UP)
            (let ((next-sel (modulo (- sel 1) ntags)))
              (update-tags sel next-sel)
              (loop next-sel)))
          ((= c KEY_DOWN)
            (let ((next-sel (modulo (+ sel 1) ntags)))
              (update-tags sel next-sel)
              (loop next-sel)))
          ((= c KEY_LEFT)
            (let ((next-sel (if (>= sel stride)
                                (- sel stride)
                                (min (+ sel (- ntags (modulo ntags stride)))
                                     (- ntags 1)))))
              (update-tags sel next-sel)
              (loop next-sel)))
          ((= c KEY_RIGHT)
            (let ((next-sel (cond ((< (+ sel stride) ntags)
                                    (+ sel stride))
                                  ((< sel (- ntags (modulo ntags stride)))
                                    (- ntags 1))
                                  (else (modulo sel stride)))))
              (update-tags sel next-sel)
              (loop next-sel)))
          ((= c 32)
            (vector-set! state sel (- 1 (vector-ref state sel)))
            (show-tag sel sel)
            (loop sel))
          ((= c 10)
            (let result ((index 0) (add '()) (del '()))
              (cond
                ((>= index ntags)
                  (list add del))
                ((= (cadddr (vector-ref tags index)) (vector-ref state index))
                  (result (+ index 1) add del))
                ((zero? (vector-ref state index))
                  (result (+ index 1) add
                          (cons (cadr (vector-ref tags index)) del)))
                (else
                  (result (+ index 1)
                          (cons (cadr (vector-ref tags index)) add)
                          del)))))
          ((= c 27) '(()()))
          ((or (<= 65 c 90) (<= 97 c 122))
            (let search ((prev-sel sel)
                         (prev-ch  (char->integer (string-ref
                                           (cadr (vector-ref tags sel)) 0))))
              (let* ((next-sel (modulo (+ prev-sel 1) ntags))
                     (next-ch  (char->integer (string-ref
                                       (cadr (vector-ref tags next-sel)) 0))))
                (cond
                  ((= next-sel sel)
                    (loop sel))
                  ((or (= next-ch c) (< prev-ch c next-ch))
                    (update-tags sel next-sel)
                    (loop next-sel))
                  (else (search next-sel next-ch))))))
          (else (mvprintw (+ 1 stride) 0 "~S ~S" KEY_DOWN c) (loop sel)))))))

(define (select-tags* entry-id)
  (if (update-allowed? entry-id)
    (let ((tags (list->vector (query
                   (map-rows* (lambda (id name count active)
                                (list id name (conc " (" count ")")
                                      active)))
                   (sql db
                    "SELECT id,name,COUNT(url_id),COALESCE(MAX(url_id==?),0)
                     FROM tag LEFT OUTER JOIN tagrel ON tag_id=tag.id
                     GROUP BY tag.name;")
                   entry-id))))
      (dynamic-wind initscr (lambda () (select-tags** entry-id tags)) endwin))
    '(()())))

(defcmd (select-tags . args)
  "[[mtime] entry-id]" "Interactively select tags using dialog(1)"
  (let* ((entry-id (case (length args)
                     ((0) cur-entry)
                     ((1) (car args))
                     ((2) (cadr args))
                     (else
                       (assert #f "Too many arguments to select-tags " args))))
         (mtime    (if (= 2 (length args)) (car args) (current-seconds)))
         (changes  (select-tags* entry-id))
         (added    (car changes))
         (removed  (cadr changes)))
    (unless-protected entry-id
      (untag* (- mtime 1) entry-id removed)
      (tag* mtime entry-id added))))

;;;;;;;;;;;;;;;;;;;;;
;; Gruik Management

(define (pull-gruiks* mtime mark)
  (let ((last-id (query fetch-value (sql db "SELECT MAX(id) FROM entry;"))))
    (exec
      (sql db "INSERT OR IGNORE
                 INTO entry(url,type,description,notes,ctime,mtime)
               SELECT url,
                      CASE WHEN description IS NULL THEN NULL
                           WHEN substr(description,1,1)='<' THEN 'html'
                           WHEN substr(description,1,3)=' - '
                            OR substr(description,1,3)=' + ' THEN 'markdown-li'
                           ELSE 'text' END,
                      trim(description,char(10))||char(10),
                      trim(notes,char(10))||char(10),
                      stime,?
               FROM gruik
               WHERE mark=? AND url NOT IN (SELECT url FROM entry);")
      mtime
      mark)
    (exec
      (sql db "INSERT OR IGNORE INTO tagrel(url_id,tag_id)
               SELECT entry.id,tag_id
               FROM gruik_tags LEFT OUTER JOIN gruik ON gruik_id = gruik.id
                               LEFT OUTER JOIN entry ON gruik.url = entry.url
               WHERE gruik.mark=?;")
      mark)
    (exec
      (sql db "DELETE FROM gruik WHERE mark=?;")
      mark)
    (print-selection (conc "WHERE entry.id > " last-id)))
    (update-feed-cache mtime))

(defcmd (pull-gruiks mark)
  "mark" "import gruiks at the given mark level"
  (let* ((wh (conc "WHERE url IN (SELECT url FROM gruik WHERE mark=" mark ")"))
         (n  (query fetch-value
               (sql/transient db (conc "SELECT COUNT(id) FROM entry " wh)))))
    (if (zero? n)
        (pull-gruiks* (current-seconds) mark)
        (begin
          (write-line (conc vt100-alert "Conflicting gruiks:" vt100-reset))
          (query
            (for-each-row* (lambda (id url notes)
              (write-line (conc id " - " vt100-entry-header url vt100-reset))
              (write-line notes)))
            (sql db "SELECT id,url,notes FROM gruik
                     WHERE mark=? AND url IN (SELECT url FROM entry);")
            mark)
          (write-line (conc vt100-alert "Conflicting entries:" vt100-reset))
          (print-selection wh)))))

(defcmd (catchup-gruik)
  "" "skip all past unfetched gruiks"
  (let ((src-path (get-config "gruik-source")))
    (write-line (conc "Before: " (get-config "gruik-seen")))
    (when src-path
      (set-config "gruik-seen" (file-size src-path)))
    (write-line (conc "After " (get-config "gruik-seen")))))

;;;;;;;;;;;;;;;;;;;;
;; Feed Generation

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

(define (write-feed mtime title self rows)
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
           ,@(map (lambda (row) (apply feed->sxml row)) rows)))
      ns-prefixes: '((*default* . "http://www.w3.org/2005/Atom")))))

(define (generate-feed forced feed-id filename url selector title mtime)
  (let* ((rows (query fetch-rows
                      (sql db (string-append "SELECT id,url,type,description,
                                                     notes,ptime,ctime,mtime
                                              FROM entry " selector ";"))))
         (generate?
           (cond ((null? rows)
                   (when config-verbose
                     (write-line (conc "Feed " feed-id " is empty")))
                   #f)
                 ((any (cut = feed-id <>) dirty-feeds)
                   (when config-verbose
                     (write-line (conc "Generating feed " feed-id)))
                   #t)
                 (forced
                   (when config-verbose
                     (write-line (conc "Generating feed " feed-id
                                       " unconditionally")))
                   #t)
                 (else
                   (when config-verbose
                     (write-line (conc "Feed " feed-id
                                       " is already up to date")))
                   #t))))
    (when generate?
      (with-output-to-file filename
        (lambda () (write-feed (if (null? mtime) (list-ref (car rows) 7) mtime)
                               title url rows)))
      (set! dirty-feeds (delete! feed-id dirty-feeds =))
      (set! feed-cache
        (alist-update! feed-id
                       (map (lambda (row) (list (car row) (list-ref row 7)))
                            rows)
                       feed-cache =)))))

(define (generate-feeds forced id-list)
  (for-each
    (lambda (row) (apply generate-feed forced row))
    (if (null? id-list)
        (query fetch-rows
               (sql db "SELECT id,filename,url,selector,title,mtime
                        FROM feed WHERE active=1;"))
        (map (lambda (id)
               (query fetch
                      (sql db "SELECT id,filename,url,selector,title,mtime
                               FROM feed WHERE id=?;")
                      id))
             id-list))))

(defcmd (force-generate . args)
  "[feed-id ...]"
  "Generate unconditionally the given feeds, or all active feeds"
  (generate-feeds #t args))

(defcmd (generate . args)
  "[feed-id ...]" "Generate if needed the given feeds, or all active feeds"
  (generate-feeds #f args))

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

(define write-each-row
  (for-each-row
    (lambda (row) (if (= 1 (length row))
                      (write-line (->string (car row)))
                      (begin (write row) (newline))))))

(define (write-query text . args)
   (apply query write-each-row (sql/transient db text) args))

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
  (string-append
    (if (null? protection-overrides)
        ""
        (string-append "!"
          (string-intersperse (map ->string protection-overrides) ",")))
    (cond ((eqv? state 'general) "> ")
          ((eqv? state 'in-command) "… ")
          (else "? "))))

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
