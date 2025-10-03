(require 'go-ts-mode)

(defcustom go-ts-mode-build-tags nil
  "List of Go build tags for the test commands."
  :type '(repeat string)
  :group 'go)

(defcustom go-ts-mode-test-flags nil
  "List of extra flags for the Go test commands."
  :type '(repeat string)
  :group 'go)

(defun go-ts-mode--get-build-tags-flag ()
  "Return the compile flag for build tags.
This function respects the `go-ts-mode-build-tags' variable for
specifying build tags."
  (if go-ts-mode-build-tags
      (format "-tags=%s" (string-join go-ts-mode-build-tags ","))
    ""))

(defun go-ts-mode--get-test-flags ()
  "Return the flags for test invocation.
This function respects the `go-ts-mode-test-flags' variable for
specifying test flags."
  (if go-ts-mode-test-flags
      (mapconcat #'shell-quote-argument go-ts-mode-test-flags " ")
    ""))

(defun my-go--get-this-package ()
  "Return package in the `default-directory'."
  (with-temp-buffer
    (call-process "go" nil '(t nil) nil "list")
    (string-trim (thing-at-point 'line t))))

(defun my-go--get-compile-test-command (&optional regexp)
  "Return command to compile the tests matching REGEXP.
this function respects `go-ts-mode-build-tags' and
`go-ts-mode-test-flags' variables for specifying build tags and test
flags."
  (let ((run (if regexp (format "-run '%s'" regexp) "")))
    (format "go test %s %s %s %s"
            (go-ts-mode--get-test-flags)
            (go-ts-mode--get-build-tags-flag)
            (my-go--get-this-package)
            run)))

(defun my-go--get-function-regexp (name)
  (if name
      (format "^%s$" name)))

(defun my-go--find-defun-at (start)
  "Return the first defun node from START."
  (let ((thing (or treesit-defun-type-regexp 'defun)))
    (or (treesit-thing-at start thing)
        (treesit-thing-next start thing))))

(defun my-go--get-function-at (start)
  "Return the name of a function at point."
  (let* ((node (my-go--find-defun-at start))
         (name (go-ts-mode--defun-name node t))
         (node-start (treesit-node-start node))
         (node-end (treesit-node-end node)))
    (cond ((or (not node)
               (> start node-end)
               (< start node-start))
           nil)
          ((not (equal (treesit-node-type node) "function_declaration"))
           nil)
          (t
           name))))

(defun my-go--get-test-regexp-at (start)
  "Return a regular expression for the tests at point."
  (if-let* ((name (my-go--get-function-at start))
            ((string-prefix-p "Test" name)))
      (my-go--get-function-regexp name)
    (user-error "No test function found")))

(defun my-go-test-function-at-point (&optional edit-command)
  "Run the unit test at point."
  (interactive "P")
  (let ((command (my-go--get-compile-test-command (my-go--get-test-regexp-at (point)))))
    (when edit-command
      (setq command (compilation-read-command command)))
    (compile command)))

(defun my-go-test-this-package (&optional edit-command)
  "Run all the unit tests under the current file's package."
  (interactive "P")
  (let ((command (my-go--get-compile-test-command)))
    (when edit-command
      (setq command (compilation-read-command command)))
    (compile command)))

(defun my-go-debug-test-function-at-point (&optional edit-regexp)
  "Debug the unit test at point."
  (interactive "P")
  (require 'dape)
  (let* ((test-regexp (my-go--get-test-regexp-at (point)))
         (build-tags-flag (go-ts-mode--get-build-tags-flag))
         (build-flags (if (not (string-empty-p build-tags-flag)) (vector build-tags-flag) `[]))
         (package-name (my-go--get-this-package)))
    (when edit-regexp
      (setq test-regexp (read-string "Regexp: " test-regexp)))
    (dape (dape--config-eval 'dlv `(:mode "test"
                                    :program ,package-name
                                    :args ["-test.run" ,test-regexp]
                                    :buildFlags ,build-flags)))))

(provide 'my-go)
