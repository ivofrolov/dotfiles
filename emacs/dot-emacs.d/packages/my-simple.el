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

(defun move-line-down()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun back-to-indentation-or-beginning ()
  (interactive "^")
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(defun shift-text (N)
  (if (not (use-region-p))
      (indent-rigidly (line-beginning-position) (line-end-position) N)
    (let ((mark (mark)))
      (save-excursion
        (indent-rigidly (region-beginning) (region-end) N)
        (set-mark mark)
        (setq deactivate-mark nil)))))

(defun shift-left ()
  "Shift text left `tab-width' columns."
  (interactive)
  (shift-text (- tab-width)))

(defun shift-right ()
  "Shift text right `tab-width' columns."
  (interactive)
  (shift-text tab-width))

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
            (if (< arg 0)
                (move-beginning-of-line (+ arg 1))
                (move-end-of-line (+ arg 1)))
            (point))))
        (t
         (push-mark (point) nil)
         (beginning-of-line)
         (push-mark
          (save-excursion
            (move-end-of-line (prefix-numeric-value arg))
            (point))
          nil t))))

(provide 'my-simple)
