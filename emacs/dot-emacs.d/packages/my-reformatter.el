(require 'reformatter)

(defvar black-format-args
  '("-q" "-")
  "Black config.")

(defvar ruff-format-args
  '("format" "-q" "-")
  "Ruff Formatter config.")

(defvar astyle-format-args
  ;; tries to follow GNU coding standards by default
  '("--style=gnu" "--max-code-length=79" "--indent=spaces=2" "-n")
  "Artistic Style config.")

;;;###autoload (autoload 'black-format-buffer "current-file" nil t)
;;;###autoload (autoload 'black-format-region "current-file" nil t)
;;;###autoload (autoload 'black-format-on-save-mode "current-file" nil t)
(reformatter-define black-format
  :program "black"
  :args black-format-args
  :lighter " Black")

;;;###autoload (autoload 'ruff-format-buffer "current-file" nil t)
;;;###autoload (autoload 'ruff-format-region "current-file" nil t)
;;;###autoload (autoload 'ruff-format-on-save-mode "current-file" nil t)
(reformatter-define ruff-format
  :program "ruff"
  :args ruff-format-args
  :lighter " Ruff")

;;;###autoload (autoload 'astyle-format-buffer "current-file" nil t)
;;;###autoload (autoload 'astyle-format-region "current-file" nil t)
;;;###autoload (autoload 'astyle-format-on-save-mode "current-file" nil t)
(reformatter-define astyle-format
  :program "astyle"
  :args astyle-format-args
  :lighter " Astyle")

(provide 'my-reformatter)
