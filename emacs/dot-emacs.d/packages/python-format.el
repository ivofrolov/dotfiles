(require 'reformatter)

;;;###autoload (autoload 'python-format-buffer "current-file" nil t)
;;;###autoload (autoload 'python-format-region "current-file" nil t)
;;;###autoload (autoload 'python-format-on-save-mode "current-file" nil t)
(reformatter-define python-format
  :program "black"
  :args '("-S" "-q" "-")
  :lighter " Black")

(provide 'python-format)
