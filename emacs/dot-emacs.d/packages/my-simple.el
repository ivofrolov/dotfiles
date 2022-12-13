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

(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(defun shift-left ()
  (interactive)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly-left-to-tab-stop (region-beginning)
                                           (region-end))
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly-left-to-tab-stop (line-beginning-position)
                                     (line-end-position))))

(defun shift-right ()
  (interactive)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly-right-to-tab-stop (region-beginning)
                                            (region-end))
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly-right-to-tab-stop (line-beginning-position)
                                      (line-end-position))))

(provide 'my-simple)
