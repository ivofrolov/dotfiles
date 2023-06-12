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

(defun back-to-indentation-or-beginning ()
  (interactive "^")
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

(defun mark-line (&optional arg allow-extend)
  "Set mark ARG lines. Derived from `mark-word'"
  (interactive "P\np")
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (region-active-p)))
         (setq arg (if arg (prefix-numeric-value arg)
                     (if (< (mark) (point)) -1 1)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (forward-line arg)
            (point))))
        (t
         (beginning-of-line)
         (push-mark
          (save-excursion
            (forward-line (prefix-numeric-value arg))
            (point))
          nil t))))

(provide 'my-simple)
