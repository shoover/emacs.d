;;; sync-model-test.el --- Tests for sync model helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'sync-model)

(ert-deftest org-rem-parse-title-and-tags-trailing-only ()
  (let ((parsed (org-rem-parse-title-and-tags "Pay rent #finance #home")))
    (should (equal (plist-get parsed :title) "Pay rent"))
    (should (equal (plist-get parsed :tags) '("finance" "home")))))

(ert-deftest org-rem-parse-title-and-tags-ignores-middle-hash-tokens ()
  (let ((parsed (org-rem-parse-title-and-tags "Fix #42 bug #work")))
    (should (equal (plist-get parsed :title) "Fix #42 bug"))
    (should (equal (plist-get parsed :tags) '("work")))))

(ert-deftest org-rem-render-title-and-tags-normalizes-tags ()
  (should
   (equal (org-rem-render-title-and-tags "Pay rent" '("Home" "finance" "home"))
          "Pay rent #finance #home")))

(ert-deftest org-rem-canonical-hash-normalizes-notes-and-tags ()
  (let* ((item-a '((title . "Pay rent")
                   (tags . ("home" "finance"))
                   (todo_state . "next")
                   (scheduled . ((year . 2026) (month . 2) (day . 15)))
                   (notes . "line one\nline two\n")
                   (url . "https://example.com ")
                   (completed . :json-false)))
         (item-b '((title . "Pay rent")
                   (tags . ("finance" "home"))
                   (todo_state . "next")
                   (scheduled . ((year . 2026) (month . 2) (day . 15)))
                   (notes . "line one\r\nline two")
                   (url . "https://example.com")
                   (completed . :json-false))))
    (should (equal (org-rem-canonical-hash item-a)
                   (org-rem-canonical-hash item-b)))))

(provide 'sync-model-test)

;;; sync-model-test.el ends here
