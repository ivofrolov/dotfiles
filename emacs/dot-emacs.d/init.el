;; initialize packages
(require 'package)
(setq package-native-compile t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'load-path (locate-user-emacs-file "packages"))

;; ensure use-package
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

;; modus-themes
(use-package emacs
  :init
  (setq modus-themes-region '(no-extend))
  :config
  (load-theme 'modus-operandi)
  :bind ("<f5>" . modus-themes-toggle))

;; font
(use-package frame
  :init
  (setq default-frame-font "Hack 13")
  (defun my-server-configure-font ()
    (set-frame-font default-frame-font nil t)
    (remove-hook 'server-after-make-frame-hook #'my-server-configure-font))
  (add-hook 'server-after-make-frame-hook #'my-server-configure-font)
  :config
  (set-frame-font default-frame-font nil t))

;; cursor
(use-package emacs
  :init
  (setq-default cursor-type 'bar)
  :config
  (blink-cursor-mode 0))

;; misc
(use-package emacs
  :init
  (setq mode-line-compact 'long
        use-short-answers t
        ns-use-proxy-icon nil)
  :bind (("s-z" . undo-only)
         ("s-Z" . undo-redo)
         ("M-z" . zap-up-to-char)
         ("C-S-k" . kill-whole-line)
         ("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim)
         ("M-c" . capitalize-dwim)))

;; line wrapping
(use-package emacs
  :init
  (setq-default truncate-lines t)
  (setq visual-line-fringe-indicators '(nil right-curly-arrow))
  :hook ((text-mode help-mode) . visual-line-mode))

;; line:column numbers
(use-package emacs
  :init
  (setq display-line-numbers-type 'relative)
  (setq mode-line-position-column-format '(" C%C")
        mode-line-position-column-line-format '(" (%l,%C)"))
  :hook ((prog-mode conf-mode) . display-line-numbers-mode)
  :config
  (column-number-mode))

;; indent
(use-package emacs
  :init
  (setq-default indent-tabs-mode nil
                standard-indent 4
                tab-width 4
                tab-stop-list '(0 4))
  (defun my-default-comment-indenting ()
    (setq-local comment-column 0))
  :hook (prog-mode . my-default-comment-indenting))

(use-package ispell
  :init
  ;; hunspell dictionary located at ~/Library/Spelling/ru_RU.{aff,dic}
  (setenv "DICTIONARY" "ru_RU"))

(use-package window
  :init
  (setq split-width-threshold 144)
  :bind (("s-{" . previous-buffer)
         ("s-}" . next-buffer)
         ("s-w" . kill-current-buffer)
         ("s-W" . delete-frame)))

(use-package mouse
  :config
  (context-menu-mode))

(use-package mwheel
  :init
  (setq mouse-wheel-tilt-scroll t
        mouse-wheel-flip-direction t))

(use-package files
  :init
  (setq create-lockfiles nil
        make-backup-files nil)
  (if (eq system-type 'darwin)
      ;; macOS Trash support is in master branch so next option can be set to default a while
      ;; https://github.com/emacs-mirror/emacs/blob/d5ee49c25c8f59ab17c40eebdf38a769c2f5588b/src/nsfns.m#L2462
      (setq trash-directory "~/.Trash"))
  (setq delete-by-moving-to-trash t))

(use-package vc-hooks
  :init
  (setq vc-follow-symlinks t))

(use-package saveplace
  :config
  (save-place-mode))

(use-package my-project
  :demand t
  :config
  (setq frame-title-format '("%b" (:eval (my-current-project-file-suffix))))
  :bind (:map project-prefix-map
              ("S" . my-project-vc-create-branch-from-default)))

(use-package python
  :init
  (use-package my-reformatter)
  (use-package my-simple
    :config
    (add-font-lock-maximum-decoration '(python-mode . 2)))
  (setq python-fill-docstring-style 'pep-257-nn
        python-indent-def-block-scale 1)
  (defun my-python-mode-locals ()
    (setq-local comment-inline-offset 2
                imenu-create-index-function #'python-imenu-create-flat-index))
  :hook (python-mode . my-python-mode-locals)
  :bind (:map python-mode-map
              ("C-c C-h" . python-eldoc-at-point)
              ("C-c C-f" . black-format-buffer)))

(use-package cc-mode
  :init
  (use-package my-reformatter)
  (defun my-c-mode-locals ()
    (setq-local comment-style 'multi-line))
  :hook (c-mode . my-c-mode-locals)
  :bind (:map c-mode-map
              ("C-c C-f" . astyle-format-buffer)))

(use-package sgml-mode
  :init
  (setq sgml-basic-offset 4))

(use-package emmet-mode
  :ensure t
  :hook ((sgml-mode . emmet-mode)
         (css-mode . emmet-mode)))

(use-package paren
  :init
  (setq show-paren-when-point-inside-paren t))

(use-package subword
  :config
  (global-subword-mode))

(use-package elec-pair
  :config
  (electric-pair-mode))

(use-package delsel
  :config
  (delete-selection-mode))

(use-package icomplete
  :init
  (setq completions-detailed t)
  (setq completion-category-overrides
        '((file (styles . (basic partial-completion flex)))
          (project-file (styles . (basic substring partial-completion flex)))
          (imenu (styles . (basic substring flex)))))
  :config
  ;; also sets flex completion style
  (fido-vertical-mode))

(use-package isearch
  :init
  (setq isearch-repeat-on-direction-change t))

(use-package quail-russian-macintosh
  :config
  (setq default-input-method "russian-macintosh"))

(use-package select
  :init
  (setq select-enable-clipboard nil
        select-enable-primary t)
  :bind (("s-c" . clipboard-kill-ring-save)
         ("s-x" . clipboard-kill-region)
         ("s-v" . clipboard-yank)))

(use-package my-simple
  :bind (("C--" . pop-local-mark)
         ("C-s--" . pop-global-mark)
         ("M-o" . split-line-at-the-beginning)
         ("s-<return>" . add-line)
         ("s-[" . shift-left)
         ("s-]" . shift-right)
         ("C-a" . back-to-indentation-or-beginning)))

(use-package uniquify
  :init
  (set-default 'uniquify-buffer-name-style 'forward))

(use-package flymake
  :config
  (setq flymake-suppress-zero-counters t
        flymake-fringe-indicator-position nil
        flymake-error-bitmap '(exclamation-mark modus-themes-fringe-red)
        flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow)
        flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package diminish
  :ensure t)

;; (use-package tree-sitter
;;   :ensure t
;;   :hook
;;   (tree-sitter-after-on . tree-sitter-hl-mode)
;;   :init
;;   (global-tree-sitter-mode)
;;   )

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter
;;   :config
;;   (add-function :before-while tree-sitter-hl-face-mapping-function
;;                 (lambda (capture-name)
;;                   (string= capture-name "keyword"))))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

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

(use-package corfu
  :ensure t
  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  :init
  (global-corfu-mode))

(use-package diff-hl
  :ensure t
  :config
  (setq diff-hl-draw-borders nil)
  :init
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :ensure t
  :init
  (setq  mc/match-cursor-style nil)
  :bind (("s-d" . mc/mark-next-like-this)
         ("M-s-d" . mc/skip-to-next-like-this)
         ("s-D" . mc/mark-all-dwim)
         ("s-<mouse-1>" . mc/add-cursor-on-click)))

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
  :ensure t
  :init
  (setq json-encoding-default-indentation "    "))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package magit
  :ensure t
  :init
  (setq magit-bind-magit-project-status nil)
  :bind (:map project-prefix-map
              ("m" . magit-project-status)))

(use-package denote
  :ensure t
  :init
  (setq denote-directory "~/Documents/Notes"
        denote-dired-directories (list denote-directory)
        denote-file-type 'markdown-toml
        denote-known-keywords nil)
  :hook (dired-mode . denote-dired-mode-in-directories))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (auto-package-update-maybe))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
