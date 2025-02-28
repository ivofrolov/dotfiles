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

(defun define-my-fringe-bitmaps ()
  (define-fringe-bitmap 'my-left-arrow-half-width
    (eval-when-compile
      (fringe-helper-convert
       "........"
       "...X...."
       "..X....."
       ".X......"
       ".X......"
       "..X....."
       "...X...."
       "........")))
  (define-fringe-bitmap 'my-right-arrow-half-width
    (eval-when-compile
      (fringe-helper-convert
       "........"
       ".X......"
       "..X....."
       "...X...."
       "...X...."
       "..X....."
       ".X......"
       "........")))
  (define-fringe-bitmap 'my-ellipsis-half-width
    (eval-when-compile
      (fringe-helper-convert
       ".XX....."
       ".XX....."
       "........"
       ".XX....."
       ".XX....."
       "........"
       ".XX....."
       ".XX.....")))
  (define-fringe-bitmap 'my-exclamation-mark-half-width
    (eval-when-compile
      (fringe-helper-convert
       ".XX....."
       ".XX....."
       ".XX....."
       ".XX....."
       ".XX....."
       "........"
       ".XX....."
       ".XX.....")))
  (define-fringe-bitmap 'my-question-mark-half-width
    (eval-when-compile
      (fringe-helper-convert
       ".XX....."
       "X..X...."
       "...X...."
       "..X....."
       ".XX....."
       "........"
       ".XX....."
       ".XX.....")))
  (define-fringe-bitmap 'my-dash-half-width
    (eval-when-compile
      (fringe-helper-convert
       "........"
       "........"
       "........"
       "XXXX...."
       "XXXX...."
       "........"
       "........"
       "........")))
  (define-fringe-bitmap 'my-horizontal-bar-half-width
    (eval-when-compile
      (fringe-helper-convert
       "........"
       "........"
       "........"
       "........"
       "........"
       "........"
       "XXXX...."
       "XXXX....")))
  (define-fringe-bitmap 'my-square-half-width
    (eval-when-compile
      (fringe-helper-convert
       "........"
       "........"
       "XXXX...."
       "XXXX...."
       "XXXX...."
       "XXXX...."
       "........"
       "........")))
  (define-fringe-bitmap 'my-left-triangle-half-width
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
  (define-fringe-bitmap 'my-right-triangle-half-width
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
