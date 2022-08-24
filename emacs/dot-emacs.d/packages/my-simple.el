(require 'simple)

(defun add-font-lock-maximum-decoration (level)
  (when (nlistp font-lock-maximum-decoration)
    (setq font-lock-maximum-decoration
          (list (cons t (default-value font-lock-maximum-decoration)))))
  (add-to-list 'font-lock-maximum-decoration level))

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
