(require 'simple)

(defun pop-local-mark ()
  (interactive)
  (set-mark-command t))

(defun split-line-at-the-beginning ()
  (interactive)
  (beginning-of-visual-line)
  (split-line))

(defun add-line ()
  (interactive)
  (move-end-of-line 1)
  (default-indent-new-line))

(provide 'my-simple)
