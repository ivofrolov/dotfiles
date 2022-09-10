(require 'reformatter)

(defvar black-format-args
  '("-S" "-q" "-")
  "Black config.

Don't normalize string quotes or prefixes by default.")

(defvar astyle-format-args
  '("--style=gnu" "--max-code-length=79" "--indent=spaces=2" "-n")
  "Artistic Style config.

Tries to follow GNU coding standards by default.")

;;;###autoload (autoload 'black-format-buffer "current-file" nil t)
;;;###autoload (autoload 'black-format-region "current-file" nil t)
;;;###autoload (autoload 'black-format-on-save-mode "current-file" nil t)
(reformatter-define black-format
  :program "black"
  :args black-format-args
  :lighter " Black")

;;;###autoload (autoload 'astyle-format-buffer "current-file" nil t)
;;;###autoload (autoload 'astyle-format-region "current-file" nil t)
;;;###autoload (autoload 'astyle-format-on-save-mode "current-file" nil t)
(reformatter-define astyle-format
  :program "astyle"
  :args astyle-format-args
  :lighter " Astyle")

(provide 'my-reformatter)
