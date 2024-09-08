(require 'reformatter)

(defvar astyle-format-args
  '("--style=attach" "--max-code-length=79" "--indent=spaces=2" "-n")
  "Artistic Style config.")

;;;###autoload (autoload 'astyle-format-buffer "current-file" nil t)
;;;###autoload (autoload 'astyle-format-region "current-file" nil t)
;;;###autoload (autoload 'astyle-format-on-save-mode "current-file" nil t)
(reformatter-define astyle-format
  :program "astyle"
  :args astyle-format-args
  :lighter " Astyle")


(defvar black-format-args
  '("-q" "-")
  "Black config.")

;;;###autoload (autoload 'black-format-buffer "current-file" nil t)
;;;###autoload (autoload 'black-format-region "current-file" nil t)
;;;###autoload (autoload 'black-format-on-save-mode "current-file" nil t)
(reformatter-define black-format
  :program "black"
  :args black-format-args
  :lighter " Black")


(defvar go-format-args
  '()
  "Go fmt config.")

;;;###autoload (autoload 'go-format-buffer "current-file" nil t)
;;;###autoload (autoload 'go-format-region "current-file" nil t)
;;;###autoload (autoload 'go-format-on-save-mode "current-file" nil t)
(reformatter-define go-format
  :program "gofmt"
  :args go-format-args
  :lighter " gofmt")


(defvar ruff-format-args
  '("format" "-")
  "Ruff Formatter config.")

;;;###autoload (autoload 'ruff-format-buffer "current-file" nil t)
;;;###autoload (autoload 'ruff-format-region "current-file" nil t)
;;;###autoload (autoload 'ruff-format-on-save-mode "current-file" nil t)
(reformatter-define ruff-format
  :program "ruff"
  :args ruff-format-args
  :lighter " Ruff")

(provide 'my-reformatter)
