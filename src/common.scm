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

;;;;;;;;;;;;;;;;;;;
;; Misc Utilities

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database Creation/Migration

(define (db-version)
  (query fetch-value (sql db "PRAGMA user_version;")))

(when (null? (schema db))
  (write-line "Initializing database with schema v2")
  (for-each
    (lambda (s) (exec (sql/transient db s)))
    (list "CREATE TABLE config (key TEXT PRIMARY KEY, val);"
          "CREATE TABLE tag (id INTEGER PRIMARY KEY,
                             name TEXT NOT NULL,
                             auto INTEGER DEFAULT 0);"
          "CREATE TABLE entry (id INTEGER PRIMARY KEY,
             url TEXT NOT NULL, type TEXT, description TEXT, notes TEXT,
             title TEXT, section TEXT, section_url TEXT,
             protected INTEGER DEFAULT 0, ptime INTEGER,
             ctime INTEGER NOT NULL DEFAULT CURRENT_TIMESTAMP,
             mtime INTEGER NOT NULL DEFAULT CURRENT_TIMESTAMP);"
          "CREATE TABLE tagrel (url_id REFERENCES entry(id)
                                  ON UPDATE CASCADE ON DELETE CASCADE,
                                tag_id REFERENCES tag(id)
                                  ON UPDATE CASCADE ON DELETE CASCADE);"
          "CREATE TABLE feed (id INTEGER PRIMARY KEY, filename TEXT NOT NULL,
                              url TEXT NOT NULL, selector TEXT NOT NULL,
                              title TEXT NOT NULL,
                              active INTEGER NOT NULL DEFAULT 1,
                              mtime INTEGER);"
          "CREATE TABLE selector (id INTEGER PRIMARY KEY, text TEXT);"
          "CREATE INDEX i_mtime ON entry(mtime);"
          "CREATE INDEX i_pmtime ON entry(protected,mtime);"
          "CREATE UNIQUE INDEX i_url ON entry(url);"
          "CREATE UNIQUE INDEX i_tag ON tag(name);"
          "CREATE UNIQUE INDEX i_rel0 ON tagrel(url_id,tag_id);"
          "CREATE INDEX i_rel1 ON tagrel(url_id);"
          "CREATE INDEX i_rel2 ON tagrel(tag_id);"
          "CREATE TABLE gruik
            (id INTEGER PRIMARY KEY,
             position INTEGER NOT NULL,
             notes TEXT NOT NULL,
             description TEXT,
             ptime INTEGER NOT NULL,
             section TEXT NOT NULL,
             title TEXT NOT NULL,
             url TEXT NOT NULL,
             comment_url TEXT,
             mark INTEGER NOT NULL DEFAULT 0,
             ctime INTEGER NOT NULL,
             mtime INTEGER NOT NULL,
             stime INTEGER,
             entry_id INTEGER REFERENCES entry(id));"
          "CREATE UNIQUE INDEX i_gruik ON gruik(position);"
          "CREATE INDEX i_gruik_time ON gruik(ptime);"
          "CREATE INDEX i_gruik_url ON gruik(url);"
          "CREATE TABLE gruik_tags
            (gruik_id REFERENCES gruik(id) ON UPDATE CASCADE ON DELETE CASCADE,
             tag_id REFERENCES tag(id) ON UPDATE CASCADE ON DELETE CASCADE);"
          "CREATE UNIQUE INDEX i_gruik_rel ON gruik_tags(gruik_id,tag_id);"
          "CREATE INDEX i_gruik_tags ON gruik_tags(tag_id,gruik_id);"
          "CREATE TABLE source_rss
            (id INTEGER PRIMARY KEY,
             name TEXT NOT NULL,
             url TEXT NOT NULL);"
          "CREATE UNIQUE INDEX i_source_rss ON source_rss(name);"
          "PRAGMA user_version = 4;")))

(when (= 0 (db-version))
  (write-line "Updating database schema from v0 to v1")
  (assert (= 1 (query fetch-value
                      (sql db "SELECT val FROM config WHERE key = ?;")
                      "schema-version")))
  (for-each
    (lambda (s) (exec (sql/transient db s)))
    (list "CREATE TABLE IF NOT EXISTS
             selector (id INTEGER PRIMARY KEY, text TEXT);"
          "DELETE FROM config WHERE key='schema-version';"
          "PRAGMA user_version = 1;")))

(when (= 1 (db-version))
  (write-line "Updating database schema from v1 to v2")
  (for-each
    (lambda (s) (exec (sql/transient db s)))
    (list "ALTER TABLE feed ADD COLUMN mtime INTEGER;"
          "PRAGMA user_version = 2;")))

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
             mark INTEGER NOT NULL DEFAULT 0,
             ctime INTEGER NOT NULL,
             mtime INTEGER NOT NULL,
             stime INTEGER);"
          "CREATE UNIQUE INDEX i_gruik ON gruik(position);"
          "CREATE INDEX i_gruik_time ON gruik(ptime);"
          "CREATE TABLE gruik_tags
            (gruik_id REFERENCES gruik(id) ON UPDATE CASCADE ON DELETE CASCADE,
             tag_id REFERENCES tag(id) ON UPDATE CASCADE ON DELETE CASCADE);"
          "CREATE UNIQUE INDEX i_gruik_rel ON gruik_tags(gruik_id,tag_id);"
          "CREATE INDEX i_gruik_tags ON gruik_tags(tag_id,gruik_id);"
          "PRAGMA user_version = 3;")))

(when (= 3 (db-version))
  (for-each
    (lambda (s) (exec (sql/transient db s)))
    (list "CREATE TABLE source_rss
            (id INTEGER PRIMARY KEY,
             name TEXT NOT NULL,
             url TEXT NOT NULL);"
          "CREATE UNIQUE INDEX i_source_rss ON source_rss(name);"
          "INSERT INTO source_rss(name,url) VALUES
            ('Hacker News','https://news.ycombinator.com/rss'),
            ('Lobsters','https://lobste.rs/rss');"
          "ALTER TABLE gruik ADD COLUMN entry_id INTEGER REFERENCES entry(id);"
          "PRAGMA user_version = 4;")))

(when (= 4 (db-version))
  (for-each
    (lambda (s) (exec (sql/transient db s)))
    (list "CREATE INDEX i_gruik_url ON gruik(url);"
          "ALTER TABLE gruik ADD COLUMN comment_url TEXT;"
          "UPDATE gruik
             SET comment_url=substr(notes,instr(notes,'https://news.ycombinator.com'))
             WHERE notes LIKE '%https://news.ycombinator.com%';"
          "UPDATE gruik
             SET comment_url=substr(notes,instr(notes,'https://lobste.rs'))
             WHERE notes LIKE '%https://lobste.rs%';"
          "UPDATE gruik SET mark=-10 WHERE mark=-1;"
          "PRAGMA user_version = 5;")))

(when (= 5 (db-version))
  (for-each
    (lambda (s) (exec (sql/transient db s)))
    (list "ALTER TABLE selector ADD COLUMN name TEXT;"
          "UPDATE selector SET name = text;"
          "PRAGMA user_version = 6;")))

(when (= 6 (db-version))
  (with-transaction db
    (lambda ()
      (for-each
        (lambda (s) (exec (sql/transient db s)))
        (list
          "ALTER TABLE entry ADD COLUMN title TEXT;"
          "ALTER TABLE entry ADD COLUMN source TEXT;"
          "ALTER TABLE entry ADD COLUMN source_url TEXT;"
          "UPDATE entry
           SET title=rtrim(substr(notes,
                                  instr(notes,']')+2,
                                  instr(notes,'://')-instr(notes,']')-7),
                           ' '||CHAR(10)),
               source=substr(notes,
                             instr(notes,'[')+1,
                             instr(notes,']')-instr(notes,'[')-1)
           WHERE notes GLOB '*ruikBot*';"
;          WHERE notes REGEXP '^[0-9.: <]*[GMN]ruikBot_?> \\[[^]]*\\]';"
          "UPDATE entry SET source=substr(source,1,instr(source,':')-1)
           WHERE instr(source,':')>0;"
          "UPDATE entry SET source=substr(source,1,instr(source,' - ')-1)
           WHERE instr(source,' - ')>0;"
          "UPDATE entry
           SET source_url=substr(description,
                                 instr(description,'via ['||source||']('))
           WHERE instr(description,'via ['||source||'](')>0
             AND description
                 GLOB '*(via [[]'||source||'[]](*) [Ss]ur #gcuf[fe]ed[f)]?'
             AND description
                 NOT GLOB '*(via [[]'||source||'[]](*)*) sur #gcufeed)?';"
;            AND description REGEXP '\\(via \\['||source||'\\]\\([^\\)]*\\) ([Ss]ur |via )?#g(cu|uc)f[fe]e?ed[f)]?';"
          "UPDATE entry
           SET source_url=substr(source_url,
                                 instr(source_url,'(')+1,
                                 instr(source_url,')')-instr(source_url,'(')-1)
           WHERE source_url IS NOT NULL;"
          "PRAGMA user_version = 7;")))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database Utilitities

(define (get-config key)
  (query fetch-value (sql db "SELECT val FROM config WHERE key = ?;") key))

(define (get-config/default key default-value)
  (let ((result (get-config key)))
    (if result
        result
        default-value)))

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

(define (feed->sxml entry-id-prefix id url type descr notes ptime ctime mtime)
  `(atom:entry
     (atom:id ,(string-append entry-id-prefix (number->string id)))
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

(define (optional-feed-element key value)
  (if value (list (list key value)) '()))

(define (write-feed mtime title self rows)
  (let ((author-name  (get-config/default "author-name" "Unknown Author"))
        (author-email (get-config         "author-email"))
        (author-uri   (get-config         "author-uri"))
        (id-prefix    (get-config/default "entry-id-prefix" "")))
    (write-string
      (serialize-sxml
        `(*TOP* (@ (*NAMESPACES* (atom "http://www.w3.org/2005/Atom")))
           (*PI* xml "version='1.0' encoding='utf-8'")
           (atom:feed
             (atom:title ,title)
             (atom:author
               (atom:name ,author-name)
               ,@(optional-feed-element 'atom:email author-email)
               ,@(optional-feed-element 'atom:uri   author-uri))
             (atom:id ,self)
             (atom:link (@ (rel "self") (href ,self)))
             (atom:updated ,(rfc-3339 mtime))
             ,@(map (lambda (r) (apply feed->sxml (cons id-prefix r))) rows)))
        ns-prefixes: '((*default* . "http://www.w3.org/2005/Atom"))))))

(define (feed-rows selector)
  (query fetch-rows
         (sql/transient db (string-append "SELECT id,url,type,description,
                                                  notes,ptime,ctime,mtime
                                           FROM entry " selector ";"))))

;;;;;;;;;;;;;;;;;;;
;; Feed Utilities

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
