(require 'package)
(require 'loadhist)

;;;###autoload
(defun package-unload-reinstall (pkg)
  "Unload and reinstall package PKG."
  (interactive
   (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))

(provide 'my-package)
