;;; transform-case.el --- Transform symbol case  -*- lexical-binding: t; -*-

(require 's)
(require 'thingatpt)

(defun transform-case--snake-case-p (s)
  "Is S in snake case, e.g. \"foo_bar\"."
  (let ((case-fold-search nil))
    (string-match-p "\\`[[:lower:][:digit:]_]+\\'" s)))

(defun transform-case--upper-case-p (s)
  "Is S in upper snake case, e.g. \"FOO_BAR\"."
  (let ((case-fold-search nil))
    (string-match-p "\\`[[:upper:][:digit:]_]+\\'" s)))

(defun transform-case--pascal-case-p (s)
  "Is S in pascal case, e.g. \"FooBar\"."
  (let ((case-fold-search nil))
    (and
     (string-match "[[:lower:]]" s)
     (string-match "\\`[[:upper:]][[:lower:][:upper:][:digit:]]+\\'" s))))

(defun transform-case--camel-case-p (s)
  "Is S in camel case, e.g. \"fooBar\"."
  (let ((case-fold-search nil))
    (and
     (string-match "[[:upper:]]" s)
     (string-match "\\`[[:lower:]][[:lower:][:upper:][:digit:]]+\\'" s))))

(defun transform-case--cycle (s)
  "Return S transformed to the next case style in sequence.
foo_bar => FOO_BAR => FooBar => fooBar => foo-bar"
  (cond
   ;; foo_bar => FOO_BAR
   ((transform-case--snake-case-p s)
    (s-upcase s))
   ;; FOO_BAR => FooBar
   ((transform-case--upper-case-p s)
    (s-upper-camel-case s))
   ;; FooBar => fooBar
   ((transform-case--pascal-case-p s)
    (s-lower-camel-case s))
   ;; fooBar => foo-bar
   ((transform-case--camel-case-p s)
    (s-dashed-words s))
   ;; foo-bar => foo_bar
   (t
    (s-snake-case s))))

(define-thing-chars phrase "-[:alnum:]_")

;;;###autoload
(defun transform-case ()
  "Transform symbol case at point cycling through different styles."
  (interactive)
  (let ((symbol (thing-at-point 'phrase t))
        (bounds (bounds-of-thing-at-point 'phrase)))
    (delete-region (car bounds) (cdr bounds))
    (insert (transform-case--cycle symbol))))

(provide 'transform-case)
;;; transform-case.el ends here
