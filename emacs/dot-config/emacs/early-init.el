(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 800000)))

(require 'xdg)
(defun my/ensure-emacs-dir (base)
  (let ((dir (expand-file-name (file-name-concat base "emacs"))))
    (ignore-errors (make-directory dir))
    (abbreviate-file-name dir)))
(setq user-emacs-cache-directory (my/ensure-emacs-dir (xdg-cache-home)))
(setq user-emacs-data-directory (my/ensure-emacs-dir (xdg-data-home)))
(setq user-emacs-state-directory (my/ensure-emacs-dir (xdg-state-home)))

(setq package-user-dir (file-name-concat user-emacs-data-directory "elpa"))
(startup-redirect-eln-cache (file-name-concat user-emacs-cache-directory "eln-cache"))
(add-to-list 'treesit-extra-load-path (file-name-concat user-emacs-cache-directory "tree-sitter"))

(setq inhibit-splash-screen t
      inhibit-startup-screen t)

(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(tool-bar-mode 0)
(scroll-bar-mode 0)
(horizontal-scroll-bar-mode 0)

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
