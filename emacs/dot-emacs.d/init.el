(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; initialize packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'load-path (locate-user-emacs-file "packages"))

;; theme
(use-package modus-themes
  :ensure
  :demand
  :preface
  (custom-set-faces
   ;; don't extend the region to the edge of the window
   `(region ((t :extend nil))))
  :config
  (load-theme 'modus-operandi :no-confim)
  :bind ("<f5>" . modus-themes-toggle))

;; font
(use-package frame
  :config
  (set-frame-font "Hack 13" nil t))

;; cursor
(use-package emacs
  :custom
  (cursor-type 'bar)
  :config
  (blink-cursor-mode 0))

;; scroll
(use-package emacs
  :custom
  (scroll-preserve-screen-position t))

;; undo
(use-package simple
  :custom
  (undo-no-redo t)
  :bind
  ("s-Z" . undo-redo))

;; modeline
(use-package emacs
  :custom
  (mode-line-compact 'long)
  (column-number-mode t)
  (mode-line-position-column-format '(" C%C"))
  (mode-line-position-column-line-format '(" %l:%C"))
  (mode-line-percent-position nil))

(use-package display-line-numbers
  :custom
  (display-line-numbers-type 'relative)
  :hook ((prog-mode conf-mode) . display-line-numbers-mode))

;; misc
(use-package emacs
  :init
  (setq ns-use-proxy-icon nil)
  :custom
  (use-system-tooltips nil)
  (use-short-answers t)
  (ring-bell-function 'ignore)
  :bind (("M-z" . zap-up-to-char)
         ("C-S-k" . kill-whole-line)
         ("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim)
         ("M-c" . capitalize-dwim)
         ("s-k" . duplicate-dwim)))

;; line wrapping
(use-package emacs
  :custom
  (truncate-lines t)
  (visual-line-fringe-indicators '(nil right-curly-arrow))
  :hook ((text-mode help-mode) . visual-line-mode))

;; indent
(use-package emacs
  :custom
  (indent-tabs-mode nil)
  (tab-always-indent t)
  (standard-indent 4)
  (tab-width 4)
  (tab-stop-list '(0 4))
  (comment-column 0))

(use-package ispell
  :init
  ;; hunspell dictionary located at ~/Library/Spelling/ru_RU.{aff,dic}
  (setenv "DICTIONARY" "ru_RU"))

(use-package re-builder
  :custom
  (reb-re-syntax 'string))

(use-package window
  :custom
  (display-buffer-base-action
   '((display-buffer-reuse-window display-buffer-same-window)
     (reusable-frames . t)))
  (even-window-sizes nil)
  (split-width-threshold 144)
  (switch-to-prev-buffer-skip-regexp '("\\*.+\\*" "magit"))
  :bind (("s-{" . previous-buffer)
         ("s-}" . next-buffer)
         ("s-w" . kill-current-buffer)
         ("s-W" . delete-frame)))

;; (use-package help
;;   :custom
;;   (help-window-keep-selected t))

(use-package mouse
  :config
  (context-menu-mode))

(use-package mwheel
  :custom
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-flip-direction t))

(use-package files
  :custom
  (create-lockfiles nil)
  (make-backup-files nil)
  (delete-by-moving-to-trash t)
  ;; :init
  ;; (if (eq system-type 'darwin)
  ;;     ;; macOS Trash support is in master branch so next option can be set to default a while
  ;;     ;; https://github.com/emacs-mirror/emacs/blob/d5ee49c25c8f59ab17c40eebdf38a769c2f5588b/src/nsfns.m#L2462
  ;;     (setq trash-directory "~/.Trash"))
  )

(use-package vc-hooks
  :custom
  (vc-follow-symlinks t))

(use-package saveplace
  :config
  (save-place-mode))

(use-package my-project
  :demand
  :config
  (setq frame-title-format '("%b" (:eval (my-current-project-file-suffix))))
  :bind (:map project-prefix-map
              ("S" . my-project-vc-create-branch-from-default)))

(use-package emacs
  :custom
  (delete-pair-blink-delay 0))

(use-package font-lock
  :custom
  (font-lock-maximum-decoration 2))


(use-package xref
  :custom
  (xref-history-storage #'xref-window-local-history))

(use-package treesit
  :custom
  (treesit-font-lock-level 2))

(use-package treesit-auto
  :ensure
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(use-package python
  :custom
  (python-fill-docstring-style 'pep-257-nn)
  (python-indent-def-block-scale 1)
  :preface
  (defun my-python-base-mode-locals ()
    (setq-local tab-width 4
                comment-inline-offset 2))
  (defun my-python-mode-locals ()
    (setq-local imenu-create-index-function #'python-imenu-create-flat-index))
  (defun my-python-ts-mode-locals ()
    (setq-local imenu-create-index-function #'python-imenu-treesit-create-flat-index))
  :hook
  (python-base-mode . my-python-base-mode-locals)
  (python-mode . my-python-mode-locals)
  (python-ts-mode . my-python-ts-mode-locals)
  :init
  (use-package my-reformatter)
  :config
  (define-abbrev python-base-mode-abbrev-table "ifmain"
    "" 'python-skeleton-ifmain)
  (define-skeleton python-skeleton-ifmain
    "Insert top-level code environment check"
    nil
    "if __name__ == \"__main__\":\n"
    >)
  :bind (:map python-mode-map
              ("C-c C-h" . python-eldoc-at-point)
              ("C-c C-f" . black-format-buffer)
         :map python-ts-mode-map
              ("C-c C-h" . python-eldoc-at-point)
              ("C-c C-f" . black-format-buffer)))

(use-package cc-mode
  :preface
  (defun my-c-mode-locals ()
    (setq-local comment-style 'multi-line))
  :init
  (use-package my-reformatter)
  :hook (c-mode . my-c-mode-locals)
  :bind (:map c-mode-map
              ("C-c C-f" . astyle-format-buffer)))

(use-package my-sql-presto
  :after sql)

(use-package sgml-mode
  :custom
  (sgml-basic-offset 4))

(use-package emmet-mode
  :ensure
  :hook ((sgml-mode . emmet-mode)
         (css-mode . emmet-mode)))

(use-package json-ts-mode
  :custom
  (json-ts-mode-indent-offset 4))

(use-package paren
  :custom
  (show-paren-when-point-inside-paren t))

(use-package subword
  :delight
  :config
  (global-subword-mode))

(use-package elec-pair
  :config
  (electric-pair-mode))

(use-package delsel
  :config
  (delete-selection-mode))

(use-package icomplete
  :custom
  (completions-detailed t)
  (completion-category-overrides
        '((file (styles . (basic partial-completion flex)))
          (project-file (styles . (basic substring partial-completion flex)))
          (imenu (styles . (basic substring flex)))))
  :config
  ;; also sets flex completion style
  (fido-vertical-mode))

(use-package isearch
  :custom
  (isearch-lazy-count t)
  (isearch-repeat-on-direction-change t))

(use-package quail-russian-macintosh
  :custom
  (default-input-method "russian-macintosh"))

(use-package select
  :custom
  (select-enable-clipboard nil)
  (select-enable-primary t)
  :bind (("s-c" . clipboard-kill-ring-save)
         ("s-x" . clipboard-kill-region)
         ("s-v" . clipboard-yank)))

(use-package abbrev
  :delight abbrev-mode
  :custom
  (abbrev-mode t)
  (abbrev-suggest t))

(use-package eldoc
  :custom
  (eldoc-minor-mode-string nil))

(use-package autorevert
  :config
  (global-auto-revert-mode))

(use-package simple
  :config
  (use-package my-simple
    :bind (("s-[" . pop-local-mark)
           ("M-s-[" . pop-global-mark)))
  :custom
  (mark-ring-max 6)
  (global-mark-ring-max 9))

(use-package my-simple
  :bind (("M-o" . split-line-at-the-beginning)
         ("s-<return>" . add-line)
         ("s-<" . shift-left)
         ("s->" . shift-right)
         ("C-a" . back-to-indentation-or-beginning)
         ("s-<left>" . back-to-indentation-or-beginning)))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package flymake
  :custom
  (flymake-suppress-zero-counters t)
  (flymake-fringe-indicator-position nil)
  (flymake-error-bitmap '(exclamation-mark modus-themes-fringe-red))
  (flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow))
  (flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package delight
  :ensure t)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ibuffer-vc
  :ensure
  :custom
  (ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (name 32 32 :left :elide)
           " "
           (mode 16 16 :left :elide)
           " "
           vc-relative-file)))
  :hook
  (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package corfu
  :ensure
  ;; :config
  ;; (defun corfu-enable-in-minibuffer ()
  ;;   "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  ;;   (when (where-is-internal #'completion-at-point (list (current-local-map)))
  ;;     (corfu-mode 1)))
  ;; (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  :init
  (global-corfu-mode))

(use-package diff-hl
  :ensure
  :custom
  (diff-hl-draw-borders nil)
  :init
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

(use-package expand-region
  :ensure
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :ensure
  :custom
  (mc/match-cursor-style nil)
  :preface
  (defun my-multiple-cursors-mode-locals ()
    (setq-local cursor-type 'box))
  (defun my-multiple-cursors-mode-locals-reset ()
    (kill-local-variable 'cursor-type))
  :hook((multiple-cursors-mode-enabled . my-multiple-cursors-mode-locals)
        (multiple-cursors-mode-disabled . my-multiple-cursors-mode-locals-reset))
  :bind (("s-d" . mc/mark-next-like-this)
         ("M-s-d" . mc/skip-to-next-like-this)
         ("s-D" . mc/mark-all-dwim)
         ("s-<mouse-1>" . mc/add-cursor-on-click)))

(use-package eglot
  :init
  (setq eglot-stay-out-of '(imenu))
  :custom
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (eglot-autoshutdown t))

(use-package markdown-mode
  :ensure
  :custom
  (markdown-asymmetric-header t))

(use-package elm-mode
  :ensure)

(use-package lua-mode
  :ensure)

(use-package which-key
  :ensure
  :delight which-key-mode
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode))

(use-package magit
  :ensure
  :init
  (setq magit-bind-magit-project-status nil)
  :custom
  (magit-auto-revert-mode nil)
  :bind (:map project-prefix-map
              ("m" . magit-project-status)))

(use-package denote
  :ensure
  :custom
  (denote-directory "~/Documents/Notes")
  (denote-dired-directories (list denote-directory))
  (denote-file-type 'markdown-toml)
  (denote-known-keywords nil)
  :hook (dired-mode . denote-dired-mode-in-directories))

(use-package denote-menu
  :ensure)

(use-package substitute
  :ensure
  :bind (("M-# d" . substitute-target-in-defun)
         ("M-# b" . substitute-target-in-buffer)
         ("M-# s" . substitute-target-below-point)
         ("M-# r" . substitute-target-above-point)))

(load custom-file)

(server-start)
