(require 'package)
(setq package-native-compile t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

(set-frame-font "Hack 13" nil t)
(setq modus-themes-region '(no-extend))
(load-theme 'modus-operandi)
(global-set-key (kbd "<f5>") 'modus-themes-toggle)

(setq flymake-error-bitmap '(exclamation-mark modus-themes-fringe-red)
      flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow)
      flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))

(defun my-project-name (project)
  (file-name-nondirectory (directory-file-name (project-root project))))

(defun my-current-project-file-suffix ()
  (let ((project (project-current)))
    (if (and buffer-file-name project)
        (format " — %s" (my-project-name project)))))

(setq frame-title-format '("%b" (:eval (my-current-project-file-suffix))))

(setq font-lock-maximum-decoration
      '((python-mode . 2)
        (t . t)))

(defun my-custom-python-mode ()
  (setq-local imenu-create-index-function
              #'python-imenu-create-flat-index))
(add-hook 'python-mode-hook #'my-custom-python-mode)
(setq python-fill-docstring-style 'pep-257-nn)

(setq-default cursor-type 'bar)
(blink-cursor-mode 0)

(setq mode-line-compact 'long
      mode-line-position-column-format '(" C%C")
      mode-line-position-column-line-format '(" (%l,%C)"))
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (global-display-line-numbers-mode)
(column-number-mode)

(setq visual-line-fringe-indicators '(nil right-curly-arrow))
;; (global-visual-line-mode 1)

(global-subword-mode)

(setq-default indent-tabs-mode nil
              standard-indent 4
              tab-width 4)

(delete-selection-mode 1)

(setq create-lockfiles nil
      make-backup-files nil)
(setq vc-follow-symlinks t)
(save-place-mode 1)

(setq completions-detailed t)
(fido-vertical-mode 1)

(setq split-width-threshold 144)

(setq use-short-answers t)

(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

(setq select-enable-clipboard nil
      select-enable-primary t)
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-x") 'clipboard-kill-region)
(global-set-key (kbd "s-v") 'clipboard-yank)

(global-set-key (kbd "s-Z") 'undo-redo)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-{") 'previous-buffer)
(global-set-key (kbd "s-}") 'next-buffer)
(global-set-key (kbd "s-w") 'kill-current-buffer)

(use-package uniquify
  :init
  (set-default 'uniquify-buffer-name-style 'forward))

(use-package tree-sitter
  :ensure t
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode)
  ;; :init
  ;; (global-tree-sitter-mode)
  )

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (add-function :before-while tree-sitter-hl-face-mapping-function
                (lambda (capture-name)
                  (string= capture-name "keyword"))))

(use-package ibuffer-vc
  :ensure t
  :init
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 32 32 :left :elide)
                " "
                (mode 16 16 :left :elide)
                " "
                vc-relative-file)))
  :hook
  (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;; (use-package vertico
;;   :ensure t
;;   :config
;;   (vertico-mode))

;; (use-package orderless
;;   :ensure t
;;   :init
;;   (setq orderless-matching-styles '(orderless-initialism orderless-regexp)
;;         completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         completion-category-overrides nil))

(use-package eglot
  :ensure t
  :init
  (setq eglot-ignored-server-capabilites '(:documentHighlightProvider)
        eglot-stay-out-of '(imenu)))

(use-package markdown-mode
  :ensure t)

(use-package elm-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package jq-mode
  :ensure t
  :mode "\\.jq\\'")

(use-package json-mode
  :ensure t)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
