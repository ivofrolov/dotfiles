;;; Prologue

(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-pinned-packages '((use-package . "gnu")))
(add-to-list 'load-path (locate-user-emacs-file "packages"))

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq package-native-compile t))


;;; Abbreviations

(use-package abbrev
  :delight abbrev-mode
  :custom
  (abbrev-mode t)
  (abbrev-suggest t))

(use-package dabbrev
  :custom
  (dabbrev-ignored-buffer-regexps '("^[[:blank:]]+\\*.*"
                                    "^\\*.*\\*$")))


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
    (define-my-fringe-bitmaps)
    (setq-default fringe-indicator-alist
                  '((truncation my-ellipsis-half-width my-ellipsis-half-width)
                    (continuation my-dash-half-width my-dash-half-width)
                    (overlay-arrow . my-right-triangle-half-width)
                    (up . up-arrow)
                    (down . down-arrow)
                    (top top-left-angle top-right-angle)
                    (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
                    (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
                    (empty-line . empty-line)
                    (unknown . my-question-mark-half-width)))
    (setq bookmark-fringe-mark 'my-square-half-width)))

;; line numbers
(use-package emacs
  :custom
  ;; (display-line-numbers-type 'relative)
  (display-line-numbers-width 3)
  (display-line-numbers-widen t)
  :hook ((prog-mode conf-mode) . display-line-numbers-mode))

;; line wrapping
(use-package emacs
  :custom
  (truncate-lines t)
  (truncate-partial-width-windows nil)
  ;; :config
  ;; (setq message-truncate-lines t)
  :preface
  ;; (defun my-truncate-lines-in-minibuffer ()
  ;;   (setq-local truncate-lines t))
  :hook (((text-mode help-mode) . visual-line-mode)
         ;; (minibuffer-setup . my-truncate-lines-in-minibuffer)
         ))

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
   '(region ((t :extend nil))))
  :init
  (setq modus-themes-common-palette-overrides
        '((bg-region bg-ochre)
          (fg-region unspecified)
          (comment green-faint)
          (docstring green-faint)
          (cursor maroon)))
  :config
  (load-theme 'modus-operandi :no-confim)
  :bind ("<f5>" . modus-themes-toggle))

(use-package tab-line
  :custom
  (tab-line-close-button-show nil)
  (tab-line-new-button-show nil))

(use-package frame
  :bind (("s-§" . other-frame)
         ("s-N" . make-frame)
         ("s-W" . delete-frame)))

(use-package window
  :custom
  ;; (display-buffer-base-action
  ;;  '((display-buffer-reuse-window display-buffer-same-window)
  ;;    (reusable-frames . t)))
  (display-buffer-alist
   '(("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
      (display-buffer-no-window)
      (allow-no-window . t))))
  ;; (switch-to-buffer-obey-display-actions t)
  (switch-to-prev-buffer-skip-regexp
   '("\\*Quail Completions\\*"
     "\\*envrc\\*"
     "\\*EGLOT .+ events\\*"
     "\\*.+-format errors\\*"))
  (even-window-sizes nil)
  (fit-window-to-buffer-horizontally t)
  (split-width-threshold 140)
  (split-height-threshold 70)
  (window-resize-pixelwise t)
  (max-mini-window-height 0.2)
  :bind (("s-{" . previous-buffer)
         ("s-}" . next-buffer)
         ("s-w" . kill-current-buffer)
         ("s-n" . scratch-buffer)))

(use-package my-window
  :bind (("C-x 4 o" . my-move-buffer-other-window)))


;;; Editing

;; clipboard
(use-package emacs
  :custom
  (kill-do-not-save-duplicates t)
  (kill-whole-line t)
  (select-enable-clipboard nil)
  (select-enable-primary t)
  :bind (("s-c" . clipboard-kill-ring-save)
         ("s-x" . clipboard-kill-region)
         ("s-v" . clipboard-yank)
         ("C-S-k" . kill-whole-line)))

(use-package simple
  :custom
  (delete-selection-mode t)
  (line-move-visual nil)
  (track-eol t)
  (duplicate-line-final-position -1))

(use-package my-simple
  :bind (("s-<return>" . add-line)
         ("s-k" . duplicate-dwim)
         ("s-l" . mark-line)
         ("s-<" . shift-left)
         ("s->" . shift-right)
         ("M-P" . move-line-up)
         ("M-N" . move-line-down)
         ("M-o" . split-line-at-the-beginning)
         ("M-z" . zap-up-to-char)
         ("C-S-d" . delete-pair)
         ("M-u" . upcase-dwim)
         ("M-U" . capitalize-dwim)
         ("M-l" . downcase-dwim)
         ("C-a" . back-to-indentation-or-beginning)
         ("s-<left>" . back-to-indentation-or-beginning)
         :map visual-line-mode-map
         ("C-a" . back-to-indentation-or-beginning)
         ("s-<left>" . back-to-indentation-or-beginning)))

;; completions
(use-package minibuffer
  :custom
  (read-extended-command-predicate #'command-completion-default-include-p)
  (completions-detailed t)
  (minibuffer-electric-default-mode t)
  ;; (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :hook
  (minibuffer-setup . cursor-intangible-mode))

(use-package vertico
  :ensure
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package orderless
  :ensure
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; (use-package icomplete
;;   :config
;;   (fido-vertical-mode))

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
  (default-transient-input-method "compose")
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
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("M-TAB" . corfu-complete))
  :init
  (global-corfu-mode))

(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate" :rev :newest)
  :demand
  :custom
  (combobulate-flash-node nil)
  (combobulate-proffer-allow-numeric-selection nil)
  :hook (prog-mode . combobulate-mode))

(use-package eglot
  :init
  (setq eglot-stay-out-of '(imenu))
  :config
  (cl-defmethod xref-backend-identifier-at-point
    :extra "fallback" :around ((_backend (eql eglot)))
    (let ((found (cl-call-next-method))
          (thing (thing-at-point 'symbol)))
      (if (and (equal found "LSP identifier at point") thing)
          (substring-no-properties thing)
        found)))
  :custom
  (eglot-ignored-server-capabilities '(:documentHighlightProvider
                                       :documentFormattingProvider
                                       :documentRangeFormattingProvider
                                       :documentOnTypeFormattingProvider
                                       :inlayHintProvider))
  (eglot-autoshutdown t)
  :bind
  (:map eglot-mode-map
        ("C-c ." . eglot-find-implementation)))

(use-package eldoc
  :custom
  (eldoc-minor-mode-string nil)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-prefer-doc-buffer t))

;; (use-package embark
;;   :ensure
;;   :preface
;;   (defun my-embark-bind-keys-in-fido-mode ()
;;     (bind-key "C-." #'embark-act (current-local-map))
;;     (unbind-key "C-," (current-local-map)))
;;   :hook
;;   (icomplete-minibuffer-setup . my-embark-bind-keys-in-fido-mode)
;;   :bind
;;   (("C-." . embark-act)))

(use-package flymake
  :custom
  (flymake-suppress-zero-counters t)
  (flymake-fringe-indicator-position nil)
  :bind (:map flymake-mode-map
              ("C-c C-n" . flymake-goto-next-error)
              ("C-c C-p" . flymake-goto-prev-error)))

;; mark ring
(use-package emacs
  :preface
  (defun my-delete-duplicates-from-mark-ring (&rest _)
    (set 'mark-ring (delete (mark-marker) mark-ring)))
  (advice-add 'push-mark :after #'my-delete-duplicates-from-mark-ring)
  (advice-add 'pop-mark :before #'my-delete-duplicates-from-mark-ring)
  :config
  (use-package my-simple
    :bind (("s-[" . pop-local-mark)
           ("M-s-[" . pop-global-mark)))
  :custom
  (mark-ring-max 6)
  (global-mark-ring-max 8))

(use-package mouse
  :custom
  (context-menu-mode t)
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-flip-direction t))

;; (use-package multiple-cursors
;;   :custom
;;   (mc/match-cursor-style nil)
;;   (mc/always-run-for-all t)
;;   :preface
;;   (defun my-multiple-cursors-mode-locals ()
;;     (setq-local cursor-type 'box))
;;   (defun my-multiple-cursors-mode-locals-reset ()
;;     (kill-local-variable 'cursor-type))
;;   :hook
;;   ((multiple-cursors-mode-enabled . my-multiple-cursors-mode-locals)
;;    (multiple-cursors-mode-disabled . my-multiple-cursors-mode-locals-reset))
;;   :bind
;;   (("C-;" . mc/mark-next-like-this)
;;    ("C-M-;" . mc/skip-to-next-like-this)
;;    ("s-<mouse-1>" . mc/add-cursor-on-click)
;;    :map mc/keymap
;;    ("<return>" . nil)))

;; pairs
(use-package emacs
  :custom
  (electric-pair-mode t)
  (electric-pair-skip-whitespace nil)
  (delete-pair-blink-delay 0)
  (show-paren-when-point-inside-paren t))

;; scroll
(use-package emacs
  :custom
  (scroll-preserve-screen-position t)
  (scroll-error-top-bottom t))

(use-package string-inflection
  :ensure
  :config
  (defun string-inflection-all-cycle-function (str)
    "foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => foo_bar
     foo     => FOO     => Foo    => foo"
    (cond
     ;; foo => FOO
     ((string-inflection-word-p str)
      (string-inflection-upcase-function str))
     ;; foo_bar => FOO_BAR
     ((string-inflection-underscore-p str)
      (string-inflection-upcase-function str))
     ;; FOO_BAR => FooBar
     ((string-inflection-upcase-p str)
      (string-inflection-pascal-case-function str))
     ;; FooBar => fooBar
     ;; Foo    => foo
     ((string-inflection-pascal-case-p str)
      (string-inflection-camelcase-function str))
     ;; fooBar => foo-bar
     ;; ((string-inflection-camelcase-p str)
     ;;  (string-inflection-kebab-case-function str))
     ;; foo-bar => foo_bar
     (t
      (string-inflection-underscore-function str))))
  :bind (("M-c" . string-inflection-all-cycle)))

;; (use-package substitute
;;   :ensure
;;   :bind (("M-# d" . substitute-target-in-defun)
;;          ("M-# b" . substitute-target-in-buffer)
;;          ("M-# s" . substitute-target-below-point)
;;          ("M-# r" . substitute-target-above-point)))

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
  (bookmark-save-flag nil))

(use-package files
  :custom
  (find-sibling-rules '(("\\([^/]+\\)\\.c\\'" "\\1.h")
                        ("\\([^/]+\\)\\.go\\'" "\\1_test.go")
                        ("\\([^/]+\\)_test\\.go\\'" "\\1.go")))
  :bind
  ("M-g s" . find-sibling-file))

;;; Languages

(use-package cc-mode
  :preface
  (defun my-c-mode-locals ()
    (setq-local comment-style 'multi-line))
  :init
  (use-package my-reformatter)
  :hook (c-mode . my-c-mode-locals)
  :bind (:map c-mode-map
              ("C-c C-f" . astyle-format-buffer))
  :custom
  (c-basic-offset 2)
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (csharp-mode . "csharp")
                     (other . "k&r"))))

(use-package c-ts-mode
  :preface
  (defun my-c-ts-mode-locals ()
    (setq-local comment-style 'multi-line))
  :init
  (use-package my-reformatter)
  :hook (c-ts-mode . my-c-ts-mode-locals)
  :bind (:map c-ts-mode-map
              ("C-c C-f" . astyle-format-buffer))
  :custom
  (c-ts-mode-indent-offset 2)
  (c-ts-mode-indent-style 'k&r))

(use-package d2-ts-mode)

(use-package graphql-ts-mode
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(graphql "https://github.com/bkegley/tree-sitter-graphql")))
  :custom
  (graphql-ts-indent-offset 4)
  :mode
  ("\\.graphql\\'"  . graphql-ts-mode)
  ("\\.graphqls\\'"  . graphql-ts-mode)
  ("\\.gql\\'"  . graphql-ts-mode))

(use-package haskell
  :defer
  :config
  (setq haskell-check-error-fringe nil)
  (setq haskell-check-warning-fringe nil)
  (setq haskell-check-hole-fringe nil))

(use-package elm-mode
  :defer
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
  :hook ((sgml-mode . emmet-mode)
         (css-mode . emmet-mode))
  :bind (:map emmet-mode-keymap
         ("C-M-i" . emmet-expand-line))
  :config
  (unbind-key "C-j" emmet-mode-keymap))

(use-package go-ts-mode
  :init
  (use-package my-reformatter)
  :custom
  (go-ts-mode-indent-offset 4)
  :bind
  (:map go-ts-mode-map
        ("C-c C-f" . go-format-buffer))
  :mode
  ("\\.go\\'" . go-ts-mode)
  ("/go\\.mod\\'" . go-mod-ts-mode))

(use-package json-ts-mode
  :custom
  (json-ts-mode-indent-offset 4))

(use-package jq-mode
  :mode
  ("\\.jq\\'" . jq-mode))

(use-package markdown-mode
  :custom
  (markdown-open-command "marked")
  (markdown-asymmetric-header t)
  :mode
  ("README\\.md\\'" . gfm-mode))

;; solves python shell communication issues on macos
(use-package python
  :preface
  (defun my-python-delete-eval-expressions (output)
    (string-join
     (seq-remove
      (apply-partially 'string-prefix-p "__PYTHON_EL_")
      (string-lines output nil t))))
  (defun my-inferior-python-mode-locals ()
    (setq-local comint-process-echoes t)
    (add-to-list (make-local-variable 'comint-preoutput-filter-functions)
                 #'my-python-delete-eval-expressions))
  :hook
  (inferior-python-mode . my-inferior-python-mode-locals)
  :config
  (advice-add 'python-eldoc--get-doc-at-point
              :filter-return #'my-python-delete-eval-expressions)
  :custom
  (python-shell-completion-native-enable nil))

(use-package python
  :preface
  (defun my-python-base-mode-locals ()
    (setq-local tab-width 4
                comment-inline-offset 2))
  :hook
  (python-base-mode . my-python-base-mode-locals)
  :init
  (use-package my-reformatter)
  :custom
  (python-fill-docstring-style 'pep-257-nn)
  (python-indent-def-block-scale 1)
  (python-check-command "ruff check --output-format pylint")
  (python-interpreter "python3")
  (python-shell-interpreter "python3")
  (python-flymake-command
   '("ruff" "check" "--no-fix" "--output-format" "text" "-"))
  (python-flymake-command-output-pattern
   '("^\\(?:-\\):\\(?1:[0-9]+\\):\\(?:\\(?2:[0-9]+\\):?\\)? \\(?3:.*\\)$"
     1 2 nil 3))
  (python-flymake-msg-alist
   '(("\\(^redefinition\\|.*unused.*\\|used$\\)" . :warning)
     ("\\(un-sorted\\|un-formatted\\)" . :note)))
  (python-indent-guess-indent-offset-verbose nil)
  :config
  (define-skeleton python-skeleton-ifmain
    "Insert top-level code environment check"
    nil
    "if __name__ == \"__main__\":\n"
    >)
  (define-abbrev python-base-mode-abbrev-table "ifmain"
    "" 'python-skeleton-ifmain)
  (unbind-key "C-c C-j" python-mode-map)    ; imenu
  (unbind-key "C-c C-j" python-ts-mode-map) ; imenu
  :bind (:map python-mode-map
              ("C-c C-h" . python-eldoc-at-point)
              ("C-c C-f" . ruff-format-buffer)
              :map python-ts-mode-map
              ("C-c C-h" . python-eldoc-at-point)
              ("C-c C-f" . ruff-format-buffer)))

(use-package re-builder
  :custom
  (reb-re-syntax 'string))

(use-package sql-presto
  :after sql)

(use-package sgml-mode
  :custom
  (sgml-basic-offset 4)
  :bind (:map sgml-mode-map
         ("C-M-n" . sgml-skip-tag-forward)
         ("C-M-p" . sgml-skip-tag-backward)))

(use-package typescript-ts-mode
  :mode
  ("\\.ts\\'" . typescript-ts-mode)
  ("\\.tsx\\'" . tsx-ts-mode))

(use-package zig-mode
  :defer
  :custom
  (zig-format-on-save nil))

;;; Tools

(use-package align
  :config
  (add-to-list 'align-rules-list
               '(text-numbers
                 (regexp . ".*? \\(-?[0-9.,]+[0-9., ]*\\).*")
                 (group . 1)
                 (justify . t)
                 (spacing . 0))))

(use-package avy
  :bind
  (("M-g w" . avy-goto-word-or-subword-1)
   ("C-'" . avy-goto-char-timer)
   :map isearch-mode-map
   ("C-'" . avy-isearch)))

(use-package comint
  :defer
  :custom
  (comint-input-ignoredups t))

(use-package denote
  :defer
  :custom
  (denote-directory "~/Documents/Notes")
  (denote-dired-directories (list denote-directory))
  (denote-known-keywords nil)
  :config
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories))

(use-package dired
  :custom
  (dired-listing-switches "-Ahl")
  (dired-auto-revert-buffer t)
  (dired-free-space nil)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode)))

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

;; (use-package magit
;;   :ensure
;;   :init
;;   (setq magit-bind-magit-project-status nil)
;;   :custom
;;   (magit-auto-revert-mode nil)
;;   (magit-display-buffer-function #'magit-display-buffer-fullframe-status-topleft-v1)
;;   :bind (:map project-prefix-map
;;               ("m" . magit-project-status)))

(use-package man
  :custom
  (Man-notify-method 'thrifty))

(use-package marginalia
  :ensure
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; project
(use-package my-project
  :demand
  :config
  (setq frame-title-format '("%b" (:eval (my-current-project-file-suffix))))
  :bind (:map project-prefix-map
              ("S" . my-project-vc-create-branch-from-default)))

;; package
(use-package package
  :custom
  (package-install-upgrade-built-in t)
  :config
  (use-package my-package))

;; org
(use-package org
  :custom
  (org-edit-src-content-indentation 0)
  (org-cycle-separator-lines 0)
  (org-startup-folded 'fold)
  (org-cycle-hide-block-startup nil)
  (org-cycle-hide-drawer-startup t)
  ;; (org-special-ctrl-a/e t)
  :bind
  (:map org-mode-map
        ("C-M-n" . org-next-visible-heading)
        ("C-M-p" . org-previous-visible-heading))
  :config
  (add-to-list 'org-src-lang-modes '("json" . json-ts)))

(use-package ob-d2
  :ensure
  :after org
  :config
  (add-to-list 'org-src-lang-modes '("d2" . d2-ts))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((d2 . t))))

(use-package recentf
  :custom
  (recentf-mode t))

(use-package repeat
  :custom
  (repeat-mode t)
  :bind (("C-." . repeat)
         ("C-z" . repeat-complex-command)))

(use-package savehist
  :custom
  (savehist-mode t))

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

(use-package verb
  :defer
  :custom
  (verb-tag "api")
  (verb-json-use-mode 'json-ts-mode)
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package which-key
  :ensure
  :delight which-key-mode
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode))

;; should be loaded late in startup sequence
(use-package envrc
  :ensure
  :custom
  (envrc-none-lighter nil)
  (envrc-global-mode t))


;;; Epilogue

(load custom-file :no-error-if-file-is-missing)

(server-start)
