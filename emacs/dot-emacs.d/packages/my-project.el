(require 'project)

(defun my-project-name (project)
  (file-name-nondirectory (directory-file-name (project-root project))))

(defun my-current-project-file-suffix ()
  (let ((project (project-current)))
    (if (and buffer-file-name project)
        (format " — %s" (my-project-name project)))))

(provide 'my-project)
