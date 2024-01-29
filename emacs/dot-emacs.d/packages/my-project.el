(require 'project)
(require 'vc)
(require 'vc-git)

(defun my-project-name (project)
  (file-name-nondirectory (directory-file-name (project-root project))))

(defun my-current-project-file-suffix ()
  (let ((project (project-current)))
    (if (and buffer-file-name project)
        (format " — %s" (my-project-name project)))))

(defun my-vc-git--default-branch ()
  (let* ((str (vc-git--run-command-string nil "symbolic-ref" "refs/remotes/origin/HEAD")))
    (if str
        (if (string-match "^\\(refs/remotes/origin/\\)?\\(.+\\)$" str)
            (match-string 2 str)
          str))))

(defun my-project-vc-create-branch-from-default (name)
  "Creates branch from a fresh repository state"
  (interactive "sNew branch name: ")
  (let ((dir (project-root (project-current t))))
    (vc-retrieve-tag dir (my-vc-git--default-branch))
    (vc-pull)
    (vc-create-tag dir name t)))

(defun package-unload-reinstall (pkg)
  (interactive
   (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))

(provide 'my-project)
