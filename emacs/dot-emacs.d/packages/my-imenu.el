(require 'imenu)

(defun my-imenu--flatten (list &optional prefix)
  "Flatten imenu LIST.
PREFIX is prepended in front of all items."
  (mapcan
   (lambda (item)
     (if (imenu--subalist-p item)
         (let* ((name (concat (car item)))
                (next-prefix (if prefix (concat prefix "/" name) name)))
           (my-imenu--flatten (cdr item) next-prefix))
       (list (cons
              (if prefix (concat prefix " " (car item)) (car item))
              (cdr item)))))
   list))

(provide 'my-imenu)
