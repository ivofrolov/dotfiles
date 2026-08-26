;; -*- lexical-binding: t; -*-

(require 'go-ts-mode)

(defun my-go-debug-test-function-at-point (&optional edit-regexp)
  "Debug the unit test at point."
  (interactive)
  (let* ((test-regexp (go-ts-mode--get-test-regexp-at-point))
         (build-tags-flag (go-ts-mode--get-build-tags-flag))
         (build-flags (if (not (string-empty-p build-tags-flag)) (vector build-tags-flag) `[]))
         (dape-command `(dlv
                         :mode "test"
                         :program ,default-directory
                         :buildFlags ,build-flags
                         :args ["-test.run" ,test-regexp])))
    (call-interactively 'dape)))

(provide 'my-go)
