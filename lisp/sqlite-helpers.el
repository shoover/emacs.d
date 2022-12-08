;; Usage:
;; - M-x sqlite-mode-open-file
;; - Select a query
;; - M-x sqlite-query-region-to-csv
;;
;; It always executes against the last visited sqlite-mode buffer.

(if nil
    (progn
      (sqlite--mode--csv-data "*SQLite test.db*" "select addressId as a,addressId from sessions limit 100;")


      (sqlite-query-region-to-csv)

      (comment
       "select * from sessions limit 5000;"
       "select count(*) as count from shards;"
       )
      ))


;; sqlite-mode buffer tracking

(defvar sqlite--current-db-buffer "*SQLite prod.db*"
  "Tracks the last `sqlite-mode' buffer.")

(defun save-current-sqlite-db-buffer (x)
  (let ((buf (current-buffer)))
    (when (provided-mode-derived-p (buffer-local-value 'major-mode buf)
                                   'sqlite-mode)
      (setq sqlite--current-db-buffer buf))))

(add-hook 'window-buffer-change-functions #'save-current-sqlite-db-buffer)


;; CSV formatting

(defun sqlite-mode-quote-csv-field (s) ; org-quote-csv-field
  "Quote field for inclusion in CSV material."
  (if (numberp s)
      (format "%s" s)
    (if (string-match "[\",]" s)
        (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
      s)))

(defun sqlite-mode--csvify (columns rows type)
  (insert (string-join (mapcar (lambda (elem)
                                 (propertize (sqlite-mode-quote-csv-field elem)
                                             'face 'header-line))
                               columns)
                       ","))
  (insert "\n")
  (dolist (row rows)
    (let ((start (point)))
      (insert (string-join (mapcar #'sqlite-mode-quote-csv-field row) ","))
      (put-text-property start (point) 'sqlite--row row)
      (put-text-property start (point) 'sqlite--type type)
      (insert "\n"))))


;; Query + format

(defun sqlite--mode--csv-data (db-buffer query)
  (let* (stmt
         (db (with-current-buffer db-buffer
               sqlite--db)))
    (unwind-protect
        (progn
          (setq stmt (sqlite-select db query () 'set))
          (sqlite-mode--csvify (sqlite-columns stmt)
                               (cl-loop for i from 0
                                        for row = (sqlite-next stmt)
                                        while row
                                        do (setq rowid (car row))
                                        collect row)
                               (cons 'row "xxx-table")))
      (when stmt
        (sqlite-finalize stmt)))))


;; Interactive command + temp buffer management

(defun sqlite-query-region-to-csv (start end)
  "Execute the query from the current region and write the results to a temp buffer as CSV."
  (interactive "r")

  (if (buffer-live-p sqlite--current-db-buffer)
      (progn
        (let ((query (buffer-substring-no-properties start end)))

          ;; Pop up a temp buffer with the query results in csv.
          (switch-to-buffer (generate-new-buffer-name "*sqlite-csv*"))
                                        ;(make-temp-name "sqlite")

          ;; Write to a temp buffer first to disable undo boundaries for each row
          (insert (with-temp-buffer
                    (sqlite--mode--csv-data sqlite--current-db-buffer query)
                    (buffer-string)))
          (beginning-of-buffer)))))
