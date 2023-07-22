;;; Prologue

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'load-path (locate-user-emacs-file "packages"))


;;; Abbreviations

(use-package abbrev
  :delight abbrev-mode
  :custom
  (abbrev-mode t)
  (abbrev-suggest t))


;;; Display

(use-package emacs
  :init
  (setq ns-use-proxy-icon nil)
  :custom
  (use-system-tooltips nil)
  (use-short-answers t)
  (ring-bell-function 'ignore))

;; cursor
(use-package emacs
  :custom
  (cursor-type 'bar)
  :config
  (blink-cursor-mode 0))

;; font
(use-package emacs
  :custom
  (font-lock-maximum-decoration 2)
  :config
  (set-frame-font "Hack 13" nil t))

(use-package treesit
  :custom
  (treesit-font-lock-level 2))

(use-package delight
  :ensure t)

;; (use-package diff-hl
;;   :ensure
;;   :custom
;;   (diff-hl-draw-borders nil)
;;   :init
;;   (setq diff-hl-reference-revision "origin/HEAD")
;;   (global-diff-hl-mode)
;;   (diff-hl-flydiff-mode))

(use-package fringe
  :custom
  (fringe-mode '(4 . 4)) ; half-width
  :config
  (use-package my-fringe
    :config
    (replace-fringe-indicators-to-half-width-bitmaps)))

;; line numbers
(use-package emacs
  :custom
  (display-line-numbers-type 'relative)
  :hook ((prog-mode conf-mode) . display-line-numbers-mode))

;; line wrapping
(use-package emacs
  :custom
  (truncate-lines t)
  :preface
  (defun my-truncate-lines-in-minibuffer ()
    (setq-local truncate-lines t))
  :hook (((text-mode help-mode) . visual-line-mode)
         (minibuffer-setup . my-truncate-lines-in-minibuffer)))

;; modeline
(use-package emacs
  :custom
  (mode-line-compact 'long)
  (column-number-mode t)
  (mode-line-position-column-format '(" C%C"))
  (mode-line-position-column-line-format '(" %l:%C"))
  (mode-line-percent-position nil))

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

(use-package window
  :custom
  (display-buffer-base-action
   '((display-buffer-reuse-window display-buffer-same-window)
     (reusable-frames . t)))
  ;; (display-buffer-alist
  ;;  `((,(rx (or "*Help*" "*compilation*"))
  ;;     (display-buffer-reuse-window display-buffer-pop-up-window)
  ;;     (inhibit-same-window . t))))
  ;; (switch-to-buffer-obey-display-actions t)
  (even-window-sizes nil)
  (fit-window-to-buffer-horizontally t)
  (split-width-threshold 140)
  (split-height-threshold 70)
  (window-resize-pixelwise t)
  ;; (switch-to-prev-buffer-skip-regexp '("\\*.+\\*" "magit"))
  :bind (("s-{" . previous-buffer)
         ("s-}" . next-buffer)
         ("s-w" . kill-current-buffer)
         ("s-W" . delete-frame)))


;;; Editing

;; clipboard
(use-package emacs
  :custom
  (select-enable-clipboard nil)
  (select-enable-primary t)
  :bind (("s-c" . clipboard-kill-ring-save)
         ("s-x" . clipboard-kill-region)
         ("s-v" . clipboard-yank)))

(use-package simple
  :custom
  (delete-selection-mode t)
  (line-move-visual nil)
  (track-eol t))

(use-package my-simple
  :bind (("s-<return>" . add-line)
         ("C-a" . back-to-indentation-or-beginning)
         ("s-<left>" . back-to-indentation-or-beginning)
         ("s-k" . duplicate-dwim)
         ("C-S-k" . kill-whole-line)
         ("s-l" . mark-line)
         ("s-<" . shift-left)
         ("s->" . shift-right)
         ("C-s-<up>" . move-line-up)
         ("C-s-<down>" . move-line-down)
         ("M-o" . split-line-at-the-beginning)
         ("M-z" . zap-up-to-char)
         ("M-u" . upcase-dwim)
         ("M-l" . downcase-dwim)))

(use-package icomplete
  :config
  (fido-vertical-mode))

(use-package imenu
  :custom
  (imenu-auto-rescan t)
  :config
  (use-package my-imenu
    :config
    (advice-add 'imenu--make-index-alist :filter-return 'my-imenu--flatten)))

;; indent
(use-package emacs
  :custom
  (indent-tabs-mode nil)
  (tab-always-indent t)
  (standard-indent 4)
  (tab-width 4)
  (tab-stop-list '(0 4))
  (comment-column 0)
  (sentence-end-double-space nil))

;; input
(use-package quail-russian-macintosh
  :custom
  (default-input-method "russian-macintosh"))

(use-package isearch
  :custom
  (isearch-wrap-pause 'no)
  (isearch-lazy-count t)
  (isearch-repeat-on-direction-change t)
  (search-whitespace-regexp ".*?"))

(use-package ispell
  :init
  ;; hunspell dictionary located at ~/Library/Spelling/ru_RU.{aff,dic}
  (setenv "DICTIONARY" "ru_RU"))

(use-package corfu
  :ensure
  :custom
  (corfu-cycle t)
  :preface
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-echo-delay nil
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  :hook
  (minibuffer-setup . corfu-enable-in-minibuffer)
  :init
  (global-corfu-mode))

(use-package combobulate
  ;; (package-vc-install "https://github.com/mickeynp/combobulate")
  :custom
  (combobulate-flash-node nil)
  :config
  (unbind-key "M-<up>" combobulate-key-map)
  (unbind-key "M-<down>" combobulate-key-map)
  (unbind-key "M-<left>" combobulate-key-map)
  (unbind-key "M-<right>" combobulate-key-map)
  ;; :bind (:map combobulate-key-map
  ;;             ("C-<up>" . combobulate-splice-up)
  ;;             ("C-<down>" . combobulate-splice-down)
  ;;             ("C-<left>" . combobulate-yeet-forward)
  ;;             ("C-<right>" . combobulate-yoink-forward))
  :hook ((python-ts-mode . combobulate-mode)))

(use-package eglot
  :init
  (setq eglot-stay-out-of '(imenu))
  :custom
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (eglot-autoshutdown t))

(use-package eldoc
  :custom
  (eldoc-minor-mode-string nil))

(use-package flymake
  :custom
  (flymake-suppress-zero-counters t)
  (flymake-fringe-indicator-position nil)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;; mark ring
(use-package emacs
  :config
  (use-package my-simple
    :bind (("s-[" . pop-local-mark)
           ("M-s-[" . pop-global-mark)))
  :custom
  (mark-ring-max 6)
  (global-mark-ring-max 12))

(use-package minibuffer
  :custom
  (completion-auto-help 'visible)
  (completion-auto-select 'second-tab)
  (completions-header-format nil)
  (completions-detailed t)
  (completion-category-overrides
   '((file (styles . (basic partial-completion flex)))
     (project-file (styles . (basic substring partial-completion flex)))
     (imenu (styles . (basic substring flex))))))

(use-package marginalia
  :ensure
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package mouse
  :custom
  (context-menu-mode t)
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-flip-direction t))

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

;; pairs
(use-package emacs
  :custom
  (delete-pair-blink-delay 0)
  (show-paren-when-point-inside-paren t)
  :config
  (electric-pair-mode))

;; scroll
(use-package emacs
  :custom
  (scroll-preserve-screen-position t))

(use-package string-inflection
  :ensure
  :bind (("M-c" . string-inflection-all-cycle)))

(use-package substitute
  :ensure
  :bind (("M-# d" . substitute-target-in-defun)
         ("M-# b" . substitute-target-in-buffer)
         ("M-# s" . substitute-target-below-point)
         ("M-# r" . substitute-target-above-point)))

(use-package subword
  :delight
  :config
  (global-subword-mode))

;; undo
(use-package emacs
  :custom
  (undo-no-redo t)
  :bind
  ("s-Z" . undo-redo))

(use-package xref
  :custom
  (xref-history-storage #'xref-window-local-history))

;;; Files

(use-package emacs
  :custom
  (create-lockfiles nil)
  (make-backup-files nil)
  (delete-by-moving-to-trash t)
  (save-place-mode t))

(use-package autorevert
  :custom
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode))

(use-package bookmark
  :custom
  (bookmark-save-flag 1))


;;; Languages

(use-package cc-mode
  :preface
  (defun my-c-mode-locals ()
    (setq-local comment-style 'multi-line))
  :init
  (use-package my-reformatter)
  :hook (c-mode . my-c-mode-locals)
  :bind (:map c-mode-map
              ("C-c C-f" . astyle-format-buffer)))

(use-package c-ts-mode
  :preface
  (defun my-c-ts-mode-locals ()
    (setq-local comment-style 'multi-line))
  :init
  (use-package my-reformatter)
  :hook (c-ts-mode . my-c-ts-mode-locals)
  :bind (:map c-ts-mode-map
              ("C-c C-f" . astyle-format-buffer)))

(use-package d2-ts-mode)

(use-package elm-mode
  :ensure
  :config
  ;; makes `elm-documentation-lookup' work
  (defun elm-package-latest-version (package)
    "Get the latest version of PACKAGE."
    (let ((package (-find (lambda (p)
                            (let-alist p
                              (equal .name package)))
                          elm-package--contents)))
      (if (not package)
          (error "Package not found")
        (let-alist package
          (elt .versions 0))))))

(use-package emmet-mode
  :ensure
  :hook ((sgml-mode . emmet-mode)
         (css-mode . emmet-mode)))

(use-package json-ts-mode
  :custom
  (json-ts-mode-indent-offset 4))

(use-package lua-mode
  :ensure)

(use-package markdown-mode
  :ensure
  :custom
  (markdown-asymmetric-header t))

(use-package python
  :custom
  (python-fill-docstring-style 'pep-257-nn)
  (python-indent-def-block-scale 1)
  :preface
  (defun my-python-base-mode-locals ()
    (setq-local tab-width 4
                comment-inline-offset 2))
  :hook
  (python-base-mode . my-python-base-mode-locals)
  :init
  (use-package my-reformatter)
  :config
  (define-skeleton python-skeleton-ifmain
    "Insert top-level code environment check"
    nil
    "if __name__ == \"__main__\":\n"
    >)
  (define-abbrev python-base-mode-abbrev-table "ifmain"
    "" 'python-skeleton-ifmain)
  (unbind-key "C-c C-j" python-mode-map)
  (unbind-key "C-c C-j" python-ts-mode-map)
  :bind (:map python-mode-map
              ("C-c C-h" . python-eldoc-at-point)
              ("C-c C-f" . black-format-buffer)
              :map python-ts-mode-map
              ("C-c C-h" . python-eldoc-at-point)
              ("C-c C-f" . black-format-buffer)))

(use-package re-builder
  :custom
  (reb-re-syntax 'string))

(use-package sql-presto
  :after sql)

(use-package sgml-mode
  :custom
  (sgml-basic-offset 4))


;;; Tools

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

(use-package dired
  :custom
  (dired-auto-revert-buffer t))

(use-package ediff
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package ibuffer
  :custom
  (ibuffer-display-summary nil)
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

(use-package magit
  :ensure
  :init
  (setq magit-bind-magit-project-status nil)
  :custom
  (magit-auto-revert-mode nil)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-topleft-v1)
  :bind (:map project-prefix-map
              ("m" . magit-project-status)))

(use-package man
  :custom
  (Man-notify-method 'aggressive))

;; project
(use-package my-project
  :demand
  :config
  (setq frame-title-format '("%b" (:eval (my-current-project-file-suffix))))
  :bind (:map project-prefix-map
              ("S" . my-project-vc-create-branch-from-default)))

(use-package treesit-auto
  :ensure
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package vc
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git))
  (vc-git-diff-switches '("--histogram")))

(use-package which-key
  :ensure
  :delight which-key-mode
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode))


;;; Epilogue

(load custom-file)

(server-start)
