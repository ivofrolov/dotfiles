;;; transform-number.el --- Transform number  -*- lexical-binding: t; -*-

(require 'thingatpt)

(defun increment-number (&optional n)
  "Increment number at point by N."
  (interactive "p")
  (let ((x (thing-at-point 'number t))
        (bounds (bounds-of-thing-at-point 'number)))
    (cond (x
           (delete-region (car bounds) (cdr bounds))
           (insert (number-to-string (+ x (or n 1))))))))

(provide 'transform-case)
;;; transform-number.el ends here
