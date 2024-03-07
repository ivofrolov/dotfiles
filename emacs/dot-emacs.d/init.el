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

(use-package dabbrev
  :custom
  (dabbrev-ignored-buffer-regexps '("^ \\*.*\\*$")))


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
  (display-line-numbers-type 'relative)
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
          (docstring green-faint)))
  :config
  (load-theme 'modus-operandi :no-confim)
  :bind ("<f5>" . modus-themes-toggle))

(use-package tab-line
  :custom
  (tab-line-close-button-show nil)
  (tab-line-new-button-show nil))

(use-package window
  :custom
  ;; (display-buffer-base-action
  ;;  '((display-buffer-reuse-window display-buffer-same-window)
  ;;    (reusable-frames . t)))
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
  (max-mini-window-height 0.2)
  ;; (switch-to-prev-buffer-skip-regexp '("\\*.+\\*" "magit"))
  :bind (("s-{" . previous-buffer)
         ("s-}" . next-buffer)
         ("s-w" . kill-current-buffer)
         ("s-n" . scratch-buffer)
         ("s-N" . make-frame)
         ("s-W" . delete-frame)))


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
  (track-eol t))

(use-package my-simple
  :bind (("s-<return>" . add-line)
         ("s-k" . duplicate-dwim)
         ("s-l" . mark-line)
         ("s-<" . shift-left)
         ("s->" . shift-right)
         ("C-s-<up>" . move-line-up)
         ("C-s-<down>" . move-line-down)
         ("M-o" . split-line-at-the-beginning)
         ("M-z" . zap-up-to-char)
         ("M-u" . upcase-dwim)
         ("M-U" . capitalize-dwim)
         ("M-l" . downcase-dwim)
         ("C-a" . back-to-indentation-or-beginning)
         ("s-<left>" . back-to-indentation-or-beginning)
         :map visual-line-mode-map
         ("C-a" . back-to-indentation-or-beginning)
         ("s-<left>" . back-to-indentation-or-beginning)))

(use-package minibuffer
  :custom
  (completions-detailed t)
  (minibuffer-electric-default-mode t)
  (completion-styles '(flex basic))
  (completion-category-overrides
   '((file (styles . (partial-completion))))))

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
  (combobulate-proffer-allow-numeric-selection nil)
  :config
  (defun my-combobulate-disable-default-highlights (_)
    (setq-local combobulate-highlight-queries-default nil))
  (advice-add 'combobulate-highlight-install :before 'my-combobulate-disable-default-highlights)
  (unbind-key "M-<up>" combobulate-key-map) ; combobulate-splice-up
  (unbind-key "M-<down>" combobulate-key-map) ; combobulate-splice-down
  (unbind-key "M-<left>" combobulate-key-map) ; combobulate-yeet-forward
  (unbind-key "M-<right>" combobulate-key-map) ; combobulate-yoink-forward
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)))

(use-package eglot
  :init
  (setq eglot-stay-out-of '(imenu))
  :custom
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (eglot-autoshutdown t))

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
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;; mark ring
(use-package emacs
  :preface
  (defun my-multi-pop-to-mark (oldfun &rest args)
    "When popping the mark, continue popping until the cursor actually moves.
Try the repeated popping up to 10 times."
    (let ((p (point)))
      (dotimes (i 10)
        (when (= p (point))
          (apply oldfun args)))))
  (advice-add 'pop-to-mark-command :around #'my-multi-pop-to-mark)
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

(use-package multiple-cursors
  :ensure
  :custom
  (mc/match-cursor-style nil)
  (mc/always-run-for-all t)
  :preface
  (defun my-multiple-cursors-mode-locals ()
    (setq-local cursor-type 'box))
  (defun my-multiple-cursors-mode-locals-reset ()
    (kill-local-variable 'cursor-type))
  :hook
  ((multiple-cursors-mode-enabled . my-multiple-cursors-mode-locals)
   (multiple-cursors-mode-disabled . my-multiple-cursors-mode-locals-reset))
  :bind
  (("C-;" . mc/mark-next-like-this)
   ("C-M-;" . mc/skip-to-next-like-this)
   ("s-<mouse-1>" . mc/add-cursor-on-click)
   :map mc/keymap
   ("<return>" . nil)))

;; pairs
(use-package emacs
  :custom
  (electric-pair-mode t)
  (delete-pair-blink-delay 0)
  (show-paren-when-point-inside-paren t))

;; scroll
(use-package emacs
  :custom
  (scroll-preserve-screen-position t))

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
     ((string-inflection-camelcase-p str)
      (string-inflection-kebab-case-function str))
     ;; foo-bar => foo_bar
     (t
      (string-inflection-underscore-function str))))
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
  (bookmark-save-flag nil))


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

(use-package json-ts-mode
  :custom
  (json-ts-mode-indent-offset 4))

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

(use-package zig-mode
  :defer
  :custom
  (zig-format-on-save nil))

;;; Tools

(use-package comint
  :defer
  :custom
  (comint-input-ignoredups t))

(use-package denote
  :custom
  (denote-directory "~/Documents/Notes")
  (denote-dired-directories (list denote-directory))
  (denote-file-type 'markdown-toml)
  (denote-known-keywords nil)
  :hook (dired-mode . denote-dired-mode-in-directories))

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

;; should be loaded late in startup sequence
(use-package envrc
  :ensure
  :custom
  (envrc-none-lighter nil)
  (envrc-global-mode t))


;;; Epilogue

(load custom-file)

(server-start)
