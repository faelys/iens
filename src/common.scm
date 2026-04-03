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
