(require 'simple)
(require 'cua-base)

(defun pop-local-mark ()
  "Jump to mark or to the position of the last buffer change."
  (interactive)
  (setq this-command 'pop-to-mark-command)
  (if (eq last-command 'pop-to-mark-command)
      (pop-to-mark-command)
    (let ((pos (point)))
      (cua-pop-to-last-change)
      (if (eq pos (point))
          (pop-to-mark-command)))))

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
  "Move point to the first non-whitespace character or to beginning
of current visual line.

Derived from `back-to-indentation'."
  (interactive "^")
  (let ((opoint (point)))
    (beginning-of-visual-line 1)
    (skip-syntax-forward " " (line-end-position))
    (backward-prefix-chars)
    (if (= opoint (point)) (beginning-of-visual-line 1))))

(defun shift-left (start end)
  "Shift text in the region left `tab-width' columns."
  (interactive "r")
  (indent-rigidly start end (- tab-width)))

(defun shift-right (start end)
  "Shift text in the region right `tab-width' columns."
  (interactive "r")
  (indent-rigidly start end tab-width))

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
            (if (> arg 0)
                (move-end-of-line (+ arg 1))
              (move-beginning-of-line (+ arg 1))
              (back-to-indentation))
            ;; (move-beginning-of-line (+ arg 1))
            (point))))
        (t
         (setq arg (prefix-numeric-value arg))
         (if (> arg 0)
             (back-to-indentation)
           (move-end-of-line 1))
         ;; (move-beginning-of-line 1)
         (push-mark (point) nil)
         (push-mark
          (save-excursion
            (if (> arg 0)
                (move-end-of-line arg)
              (move-beginning-of-line (+ arg 2))
              (back-to-indentation))
            ;; (move-beginning-of-line (+ (prefix-numeric-value arg) 1))
            (point))
          nil t))))

(provide 'my-simple)
