;; An interactive query extension for Emacs 29's built-in sqlite-mode.
;;
;; sqlite-mode provides an elisp interface to the sqlite3 API and buffer-local
;; database connection management. Its interactive buffer is limited to schema
;; inspection and previewing a few rows of each table. With native sqlite
;; support compiled in, the pieces are there to do much more.
;;
;; This file adds interactive query capability, in particular the ability to
;; execute any string/paragraph/region as a select query and capture results in
;; a buffer formatted as CSV.
;;
;; Queries are executed against the first available database from scanning:
;; - The last visited sqlite-mode buffer. This feature adds a hook to
;;   `window-buffer-change-functions' to track the last visited sqlite-mode
;;   buffer. Queries are made to the database connection bound to the buffer.
;; - The SQLi buffer (`sql-buffer', created by `sql-sqlite'). A new connection
;;   is made to the file in `sql-database'.

;; Usage:
;; - M-x sqlite-mode-open-file OR M-x sql-sqlite
;; - Select a query
;; - M-x sqlite-query-string-to-csv, sqlite-query-paragraph-to-csv, sqlite-query-region-to-csv
;;
;; Copyright (C) 2023 Shawn Hoover

;; sqlite-query.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License version 2 or any later
;; version.

;; sqlite-query.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; sqlite-query.el or GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;; sqlite-mode last visited buffer tracking

(defvar sqlite--current-db-buffer nil
  "Tracks the last `sqlite-mode' buffer.")

(defun save-current-sqlite-db-buffer (x)
  (let ((buf (current-buffer)))
    (when (provided-mode-derived-p (buffer-local-value 'major-mode buf)
                                   'sqlite-mode)
      (setq sqlite--current-db-buffer buf))))

(add-hook 'window-buffer-change-functions #'save-current-sqlite-db-buffer)

;; CSV formatting

(defun sqlite-mode-quote-csv-field (s) ; Borrowed from `org-quote-csv-field'
  "Quote field for inclusion in CSV material."
  (if (numberp s)
      (format "%s" s)
    (if (string-match "[\",]" s)
        (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
      s)))

(defun sqlite-mode--csvify (columns rows)
  "Inserts COLUMNS and ROWS in the current buffer formatted for CSV."
  (insert (string-join (mapcar (lambda (elem)
                                 (propertize (sqlite-mode-quote-csv-field elem)
                                             'font-lock-face 'header-line))
                               columns)
                       ","))
  (insert "\n")
  (dolist (row rows)
    (insert (string-join (mapcar #'sqlite-mode-quote-csv-field row) ","))
    (insert "\n")))


;; Query

(defun sqlite-mode--query (db-buffer query fn)
  "Executes QUERY as a select against DB-BUFFER's local DB
connection. Results are passed to FN, a function of two
arguments, COLUMNS and ROWS."
  (let* (stmt
         (db (with-current-buffer db-buffer
               sqlite--db)))
    (unwind-protect
        (progn
          (setq stmt (sqlite-select db query () 'set))
          (funcall fn (sqlite-columns stmt)
                   (cl-loop for i from 0
                            for row = (sqlite-next stmt)
                            while row
                            do (setq rowid (car row))
                            collect row)))
      (when stmt
        (sqlite-finalize stmt)))))

;; Query + format

(defun sqlite-mode--csv-data (db-buffer query)
  "Executes QUERY as a select against DB-BUFFER's local DB
connection. The result is inserted in CSV format in the current
buffer."
  (sqlite-mode--query db-buffer query #'sqlite-mode--csvify))

;; Interactive query commands

(defun sqlite-query-find-db-buffer ()
  "Find a buffer with a local sqlite-mode DB connection. The last
seen sqlite-mode buffer takes precedence if there is one.
Otherwise, if there is a SQLi buffer (`sql-buffer', created by
`sql-sqlite', open a sqlite-mode DB connecction and set it to a
buffer-local variable."
  (cond
   ((buffer-live-p sqlite--current-db-buffer)
    sqlite--current-db-buffer)

   ((and (sql-buffer-live-p sql-buffer)
         (not (string= "" sql-database)))
    (with-current-buffer sql-buffer
      (when (not (sqlitep sqlite--db))
        (setq-local sqlite--db (sqlite-open sql-database))
        (unless (sqlitep sqlite--db)
          (error "`sqlite-open' failed to open SQLite file")))
      sql-buffer))))

(defun sqlite-query-string-to-csv (query)
    "Executes the string QUERY against the current database, as
selected by `sqlite-query-find-db-buffer'. Captures results to a
temp buffer formatted as CSV."
  (interactive "sSQL Text: ")

  (when-let ((db-buf (sqlite-query-find-db-buffer)))
    ;; Pop up a temp buffer with the query results in csv.
    (switch-to-buffer (generate-new-buffer-name "*sqlite-csv*"))

    ;; Write to a temp buffer first to disable undo boundaries for each row
    (insert (with-temp-buffer
              (sqlite-mode--csv-data db-buf query)
              (buffer-string)))
    (beginning-of-buffer)))

(defun sqlite-query-region-to-csv (start end)
  "Executes the query in the region against the current database, as
selected by `sqlite-query-find-db-buffer'. Captures results to a
temp buffer formatted as CSV."
  (interactive "r")
  (sqlite-query-string-to-csv (buffer-substring-no-properties start end)))

(defun sqlite-query-paragraph-to-csv ()
  "Executes the query in the current paragraph (like
`sql-send-paragraph') against the current database, as selected
by `sqlite-query-find-db-buffer'. Captures results to a temp
buffer formatted as CSV."
  (interactive)
  (let ((start (save-excursion
		         (backward-paragraph)
		         (point)))
	    (end (save-excursion
	           (forward-paragraph)
	           (point))))
    (sqlite-query-region-to-csv start end)))

(provide 'sqlite-query)

(when nil
  (sqlite-mode--csv-data
   "*SQLite test.db*"
   "select addressId as a, addressId from sessions limit 100;")
  (sqlite-query-region-to-csv)

  (comment
   "select * from sessions limit 5000;"
   "select count(*) as count from shards;"
   ))
