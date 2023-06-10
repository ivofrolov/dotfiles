(require 'fringe)

(defun fringe-helper-convert (&rest strings)
  "Convert STRINGS into a vector usable for `define-fringe-bitmap'.
Each string in STRINGS represents a line of the fringe bitmap.
Periods (.) are background-colored pixel; Xs are foreground
colored. The fringe bitmap always is aligned to the right. If the
fringe has half width, only the left 4 pixels of an 8 pixel
bitmap will be shown.

Source URL `https://github.com/nschum/fringe-helper.el'

For example, the following code defines a diagonal line.

\(fringe-helper-convert
  \"XX......\"
  \"..XX....\"
  \"....XX..\"
  \"......XX\"\)"
  (unless (cdr strings)
    ;; only one string, probably with newlines
    (setq strings (split-string (car strings) "\n")))
  (apply 'vector
         (mapcar (lambda (str)
                   (let ((num 0))
                     (dolist (c (string-to-list str))
                       (setq num (+ (* num 2) (if (eq c ?.) 0 1))))
                     num))
                 strings)))

(defun replace-fringe-indicators-to-half-width-bitmaps ()
  (define-fringe-bitmap 'left-arrow
    (eval-when-compile
      (fringe-helper-convert
       "...X...."
       "..X....."
       ".X......"
       "X......."
       "X......."
       ".X......"
       "..X....."
       "...X....")))
  (define-fringe-bitmap 'right-arrow
    (eval-when-compile
      (fringe-helper-convert
       "X......."
       ".X......"
       "..X....."
       "...X...."
       "...X...."
       "..X....."
       ".X......"
       "X.......")))
  (define-fringe-bitmap 'left-curly-arrow
    (eval-when-compile
      (fringe-helper-convert
       "X......."
       "X......."
       "XX......"
       ".X......"
       ".XX....."
       "..X....."
       "..XX...."
       "...X...."
       "...X....")))
  (define-fringe-bitmap 'right-curly-arrow
    (eval-when-compile
      (fringe-helper-convert
       "...X...."
       "...X...."
       "..XX...."
       "..X....."
       ".XX....."
       ".X......"
       "XX......"
       "X......."
       "X.......")))
  (define-fringe-bitmap 'left-triangle
    (eval-when-compile
      (fringe-helper-convert
       "...X...."
       "..XX...."
       ".XXX...."
       "XXXX...."
       "XXXX...."
       ".XXX...."
       "..XX...."
       "...X....")))
  (define-fringe-bitmap 'right-triangle
    (eval-when-compile
      (fringe-helper-convert
       "X......."
       "XX......"
       "XXX....."
       "XXXX...."
       "XXXX...."
       "XXX....."
       "XX......"
       "X......."))))

(provide 'my-fringe)
