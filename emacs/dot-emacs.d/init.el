;;; Prologue

(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://www.mirrorservice.org/sites/melpa.org/packages/") t)
(setq package-pinned-packages
      '((editorconfig . "nongnu")
        (faceup . "gnu")
        (use-package . "gnu")
        (which-key . "gnu")))
(add-to-list 'load-path (locate-user-emacs-file "packages"))

(exec-path-from-shell-initialize)

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq package-native-compile t))

(use-package gcmh
  :ensure
  :delight gcmh-mode
  :custom
  (gcmh-mode t))

;;; Abbreviations

(use-package abbrev
  :delight abbrev-mode
  :custom
  (abbrev-mode t)
  (abbrev-suggest t))

(use-package dabbrev
  :custom
  (dabbrev-ignored-buffer-regexps
   '("^[[:blank:]]+\\*.*"
     "^\\*.*\\*$")))

;;; Display

(use-package emacs
  :init
  (setq ns-use-proxy-icon nil
        ns-pop-up-frames nil)
  :custom
  (use-system-tooltips nil)
  (use-short-answers t)
  (ring-bell-function 'ignore))

(use-package emacs                      ; cursor
  :custom
  (cursor-type 'bar)
  (blink-cursor-mode nil))

(use-package emacs                      ; font
  :custom
  (font-lock-maximum-decoration 2)
  :config
  (set-frame-font "Hack 13" nil t))

(use-package treesit
  :config
  (setq major-mode-remap-alist
        '((c-or-c++-mode . c-or-c++-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (csharp-mode . csharp-ts-mode)
          (css-mode . css-ts-mode)
          (java-mode . java-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-json-mode . json-ts-mode)
          (html-mode . html-ts-mode)
          (python-mode . python-ts-mode)
          (ruby-mode . ruby-ts-mode)))
  (setq treesit-language-source-alist
        '((c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (go "https://github.com/tree-sitter/tree-sitter-go" "v0.23.4")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (graphql "https://github.com/bkegley/tree-sitter-graphql")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (swift "https://github.com/alex-pinkus/tree-sitter-swift" "0.7.1-with-generated-files")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
          (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))
  :custom
  (treesit-font-lock-level 2))

(use-package delight
  :ensure)

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

(use-package emacs                      ; line numbers
  :custom
  ;; (display-line-numbers-type 'relative)
  (display-line-numbers-width 3)
  (display-line-numbers-widen t)
  :hook ((prog-mode conf-mode) . display-line-numbers-mode))

(use-package emacs                      ; line wrapping
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

(use-package emacs                      ; mode line
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

;;; Editing

(use-package align
  :config
  (add-to-list 'align-rules-list
               '(text-numbers
                 (regexp . ".*? \\(-?[0-9.,]+[0-9., ]*\\).*")
                 (group . 1)
                 (justify . t)
                 (spacing . 0))))

(use-package emacs                      ; clipboard
  :custom
  (kill-do-not-save-duplicates t)
  (kill-whole-line t)
  (select-enable-clipboard nil)
  (select-enable-primary t)
  :bind (("s-c" . clipboard-kill-ring-save)
         ("s-x" . clipboard-kill-region)
         ("s-v" . clipboard-yank)
         ("C-S-k" . kill-whole-line)))

(use-package simple                     ; lines
  :custom
  (delete-selection-mode t)
  (line-move-visual nil)
  ;; (track-eol t)
  (duplicate-line-final-position -1))

(use-package my-simple                  ; text editing bindings
  :bind (("s-<return>" . add-line)
         ("s-k" . duplicate-lines)
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
(use-package emacs                      ; fill
  :custom
  (fill-column 88)
  (sentence-end-double-space nil)
  :config
  (unbind-key "C-x f")
  (unbind-key "C-x ;")
  (unbind-key "C-x C-n"))

(use-package emacs                      ; indent
  :custom
  (indent-tabs-mode nil)
  (tab-always-indent t)
  (standard-indent 4)
  (tab-width 4)
  ;; (tab-stop-list '(0 4))
  (comment-column 0))

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
  :defer
  :init
  ;; hunspell dictionary located at ~/Library/Spelling/ru_RU.{aff,dic}
  (setenv "DICTIONARY" "ru_RU"))

(use-package combobulate
  :vc (:url "https://github.com/mickeynp/combobulate" :rev :newest)
  :demand
  :custom
  (combobulate-flash-node nil)
  (combobulate-proffer-allow-numeric-selection nil)
  :hook
  (prog-mode . combobulate-mode)
  (yaml-ts-mode . combobulate-mode))

(use-package emacs                      ; mark ring
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

(use-package elec-pair
  :custom
  (electric-pair-mode t)
  (electric-pair-skip-whitespace nil)
  (delete-pair-blink-delay 0)
  (show-paren-when-point-inside-paren t))

(use-package emacs                      ; scroll
  :custom
  (scroll-preserve-screen-position t)
  (scroll-error-top-bottom t))

;; (use-package substitute
;;   :ensure
;;   :bind (("M-# d" . substitute-target-in-defun)
;;          ("M-# b" . substitute-target-in-buffer)
;;          ("M-# s" . substitute-target-below-point)
;;          ("M-# r" . substitute-target-above-point)))

(use-package subword
  :delight
  :custom
  (global-subword-mode t))

(use-package transform-case
  :bind
  ("M-c" . transform-case))

(use-package transform-number
  :bind
  ("C-=" . increment-number))

(use-package emacs                      ; undo
  :custom
  (undo-no-redo t)
  :bind
  ("s-Z" . undo-redo))

;;; Files

(use-package emacs                      ; files maintenance
  :custom
  (create-lockfiles nil)
  (make-backup-files nil)
  (delete-by-moving-to-trash t))

(use-package autorevert
  :custom
  (global-auto-revert-mode t)
  (global-auto-revert-non-file-buffers t))

(use-package bookmark
  :custom
  (bookmark-save-flag nil))

(use-package files                      ; siblings
  :custom
  (find-sibling-rules '(("\\([^/]+\\)\\.c\\'" "\\1.h")
                        ("\\([^/]+\\)\\.h\\'" "\\1.c")
                        ("\\([^/]+\\)\\.cpp\\'" "\\1.hpp")
                        ("\\([^/]+\\)\\.hpp\\'" "\\1.cpp")
                        ("\\([^/]+\\)\\.cpp\\'" "\\1.h")
                        ("\\([^/]+\\)\\.h\\'" "\\1.cpp")
                        ("\\([^/]+\\)\\.go\\'" "\\1_test.go")
                        ("\\([^/]+\\)_test\\.go\\'" "\\1.go")))
  :bind
  ("M-g s" . find-sibling-file))

(use-package saveplace
  :custom
  (save-place-mode t))

;;; Languages

(use-package cc-mode
  :defer
  :config
  (use-package my-reformatter)
  :bind (:map c-mode-map
              ("C-c C-f" . astyle-format-buffer))
  :custom
  (c-basic-offset 2)
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (csharp-mode . "csharp")
                     (other . "k&r"))))

(use-package c-ts-mode
  :defer
  :config
  (use-package my-reformatter)
  :bind (:map c-ts-base-mode-map
              ("C-c C-f" . astyle-format-buffer))
  :custom
  (c-ts-mode-indent-offset 2)
  (c-ts-mode-indent-style 'k&r)
  :mode
  ("\\.ino\\'" . c++-ts-mode))

(use-package d2-ts-mode
  :defer)

(use-package graphql-ts-mode
  :defer
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
  :defer
  :hook ((sgml-mode . emmet-mode)
         (css-mode . emmet-mode))
  :bind (:map emmet-mode-keymap
         ("C-M-i" . emmet-expand-line))
  :config
  (unbind-key "C-j" emmet-mode-keymap))

(use-package go-ts-mode
  :defer
  :config
  (use-package my-reformatter)
  (use-package my-go)
  ;; (use-package my-gud-dlv)
  :custom
  (go-ts-mode-indent-offset 4)
  :bind
  (:map go-ts-mode-map
        ("C-c C-f" . go-format-buffer)
        ("C-c t t" . my-go-test-function-at-point)
        ("C-c t p" . my-go-test-this-package)
        ;; ("C-c d d" . dlv-current-func)
        ("C-c d t" . my-go-debug-test-function-at-point))
  :mode
  ("\\.go\\'" . go-ts-mode)
  ("/go\\.mod\\'" . go-mod-ts-mode))

(use-package mhtml-mode
  :mode
  ("\\.html\\'" . mhtml-mode))

(use-package json-ts-mode
  :custom
  (json-ts-mode-indent-offset 4))

(use-package lua-ts-mode
  :mode
  ("\\.lua\\'" . lua-ts-mode))

(use-package markdown-mode
  :custom
  (markdown-open-command "marked")
  (markdown-asymmetric-header t)
  :mode
  ("README\\.md\\'" . gfm-mode))

(use-package python
  :defer
  :preface
  (defun my-python-base-mode-locals ()
    (setq-local tab-width 4
                comment-inline-offset 2))
  :hook
  (python-base-mode . my-python-base-mode-locals)
  :custom
  (python-fill-docstring-style 'pep-257-nn)
  (python-indent-def-block-scale 1)
  (python-check-command "ruff check --no-fix --output-format pylint")
  (python-interpreter "python3")
  (python-shell-interpreter "python3")
  (python-flymake-command
   '("ruff" "check" "--no-fix" "--output-format" "pylint" "-"))
  (python-flymake-command-output-pattern
   '("^\\(?:-\\):\\(?1:[0-9]+\\):\\(?:\\(?2:[0-9]+\\):?\\)? \\(?3:.*\\)$"
     1 2 nil 3))
  (python-flymake-msg-alist
   '(("\\(^redefinition\\|.*unused.*\\|used$\\)" . :warning)
     ("\\(un-sorted\\|un-formatted\\)" . :note)))
  (python-indent-guess-indent-offset-verbose nil)
  :config
  (use-package my-reformatter)
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

(use-package python                  ; solves python shell communication issues on macos
  :defer
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

(use-package re-builder
  :custom
  (reb-re-syntax 'string))

(use-package rust-ts-mode
  :mode
  ("\\.rs\\'" . rust-ts-mode))

(use-package sql-presto
  :after sql)

(use-package sgml-mode
  :custom
  (sgml-basic-offset 4)
  :bind (:map sgml-mode-map
         ("C-M-n" . sgml-skip-tag-forward)
         ("C-M-p" . sgml-skip-tag-backward)))

(use-package swift-ts-mode
  :mode
  ("\\.swift\\'" . swift-ts-mode))

(use-package typescript-ts-mode
  :mode
  ("\\.ts\\'" . typescript-ts-mode)
  ("\\.tsx\\'" . tsx-ts-mode))

(use-package yaml-ts-mode
  :defer
  :init
  (defun my-yaml-ts-mode-locals ()
    (setq-local treesit-font-lock-level 1)
    (treesit-font-lock-recompute-features))
  :hook
  (yaml-ts-mode . my-yaml-ts-mode-locals)
  :mode
  ("\\.ya?ml\\'" . yaml-ts-mode))

(use-package zig-mode
  :defer
  :custom
  (zig-format-on-save nil))

;;; Tools

(use-package avy
  :config
  (use-package my-avy)
  :bind
  (("M-g w" . avy-goto-word-or-subword-1)
   ("C-;" . avy-goto-char-timer)
   :map isearch-mode-map
   ("C-;" . avy-isearch)))

(use-package calendar
  :defer
  :custom
  (calendar-week-start-day 1))

(use-package comint
  :defer
  :custom
  (comint-input-ignoredups t))

(use-package compile
  :defer
  :init
  (defun my-recompile (&optional edit-command)
    (declare (interactive-only "use `compile' or `recompile' instead."))
    (interactive "P")
    (let ((current-prefix-arg nil))
      (if edit-command (call-interactively #'compile) (recompile))))
  :bind
  (("C-c C-c" . my-recompile)
   ("C-c C-k" . kill-compilation))
  :hook
  (compilation-mode . visual-line-mode)
  (compilation-mode . visual-wrap-prefix-mode))

(use-package corfu
  :ensure
  :custom
  (global-corfu-mode t)
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
        ("M-TAB" . corfu-complete)))

(use-package csv-mode
  :defer
  :mode
  ("\\.[Cc][Ss][Vv]\\'" . csv-mode)
  ("\\.tsv\\'" . tsv-mode))

(use-package dape
  :defer
  :custom
  (dape-buffer-window-arrangement 'gud)
  (dape-info-hide-mode-line nil)
  :config
  (remove-hook 'dape-start-hook 'dape-info))

(use-package denote
  :defer
  :custom
  (denote-directory "~/Documents/Notes")
  (denote-dired-directories (list denote-directory))
  (denote-known-keywords nil)
  (denote-history-completion-in-prompts nil)
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

(use-package doc-view
  :defer
  :custom
  (doc-view-pdf->png-converter-function 'doc-view-pdf->png-converter-mupdf)
  (doc-view-mupdf-use-svg t))

(use-package ediff
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eglot
  :defer
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
  (eglot-code-action-indications '(eldoc-hint))
  (eglot-ignored-server-capabilities '(:documentHighlightProvider
                                       :documentFormattingProvider
                                       :documentRangeFormattingProvider
                                       :documentOnTypeFormattingProvider
                                       :inlayHintProvider
                                       :semanticTokensProvider))
  (eglot-extend-to-xref t)
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0 :format full))
  :bind
  (:map eglot-mode-map
        ("C-c ." . eglot-find-implementation)
        ("C-c e" . eglot-code-actions)))

(use-package eldoc
  :custom
  (eldoc-minor-mode-string nil)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t))

(use-package eldoc-dox
  :custom
  (eldoc-box-clear-with-C-g t)
  :bind
  (("C-h ." . eldoc-box-help-at-point)))

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
  (flymake-mode-line-lighter "")
  (flymake-suppress-zero-counters nil)
  (flymake-error-bitmap '(my-exclamation-mark-half-width compilation-error))
  (flymake-warning-bitmap '(my-exclamation-mark-half-width compilation-warning))
  (flymake-note-bitmap '(my-exclamation-mark-half-width compilation-info))
  :bind (:map flymake-mode-map
              ("C-c C-n" . flymake-goto-next-error)
              ("C-c C-p" . flymake-goto-prev-error)))

(use-package gud
  :defer
  :custom
  (gud-highlight-current-line t))

(use-package ibuffer
  :defer
  :custom
  (ibuffer-display-summary nil)
  :bind ("C-x C-b" . ibuffer))

(use-package ibuffer-vc
  :defer
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

(use-package imenu
  :custom
  (imenu-auto-rescan t)
  (imenu-flatten 'prefix))

(use-package magit
  :defer
  :init
  (setq magit-auto-revert-mode nil) ; global-auto-revert-mode is enabled
  :custom
  (magit-define-global-key-bindings nil)
  (magit-bind-magit-project-status nil)
  ;; (magit-auto-revert-mode nil)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-section-visibility-indicators '((my-right-arrow-half-width . my-left-arrow-half-width) ("…" . t)))
  :bind (("C-x g" . magit-status)
         ("C-c g" . magit-dispatch)
         ("C-c f" . magit-file-dispatch)
         :map project-prefix-map
         ("m" . magit-project-status)))

(use-package man
  :custom
  (Man-notify-method 'thrifty))

(use-package marginalia
  :ensure
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :custom
  (marginalia-mode t))

(use-package package
  :custom
  (package-install-upgrade-built-in t)
  :config
  (use-package my-package))

(use-package package-vc
  :custom
  (package-vc-register-as-project nil))

(use-package org
  :defer
  :custom
  ;; (org-special-ctrl-a/e t)
  (org-use-speed-commands t)
  (org-export-backends '(html md org))
  (org-modules nil)
  (org-cycle-separator-lines 0)
  (org-startup-folded 'fold)
  (org-cycle-hide-block-startup nil)
  (org-cycle-hide-drawer-startup t)
  (org-edit-src-content-indentation 0)
  (org-babel-load-languages
   '((d2 . t)
     (emacs-lisp . t)
     (python . t)
     (shell . t)))
  (org-babel-python-command-nonsession "python3")
  ;; :bind
  ;; (:map org-mode-map
  ;;       ("C-M-n" . org-next-visible-heading)
  ;;       ("C-M-p" . org-previous-visible-heading))
  :config
  (add-to-list 'org-src-lang-modes '("json" . json-ts))
  ;; see https://karthinks.com/software/it-bears-repeating/
  (defvar org-link-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-n") 'org-next-link)
      (define-key map (kbd "C-p") 'org-previous-link)
      map))
  (dolist (cmd '(org-next-link org-previous-link))
    (put cmd 'repeat-map 'org-link-repeat-map)))

(use-package ob-d2
  :after org
  :config
  (add-to-list 'org-src-lang-modes '("d2" . d2-ts)))

(use-package orderless
  :ensure
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package outline
  :custom
  (outline-minor-mode-cycle t)
  (outline-minor-mode-prefix "\C-c\C-o"))

(use-package my-project
  :demand
  :init
  (defun my-project-recompile (&optional edit-command)
    (declare (interactive-only "use `project-compile' or `project-recompile' instead."))
    (interactive "P")
    (let ((current-prefix-arg nil))
      (if edit-command (call-interactively #'project-compile) (project-recompile))))
  :config
  (setq frame-title-format '("%b" (:eval (my-current-project-file-suffix))))
  :bind (:map project-prefix-map
              ("S" . my-project-vc-create-branch-from-default)
              ("c" . my-project-recompile)))

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

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package vc
  :custom
  (vc-display-status 'no-backend)
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

(use-package vertico
  :ensure
  :custom
  (vertico-mode t)
  (vertico-cycle t))

(use-package which-key
  :delight which-key-mode
  :custom
  (which-key-mode t)
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05))

(use-package xref
  :custom
  (xref-history-storage #'xref-window-local-history))

(use-package envrc                      ; should be loaded late in startup sequence
  :ensure
  :custom
  (envrc-none-lighter nil)
  (envrc-lighter nil)
  (envrc-global-mode t))

;;; Epilogue

(load custom-file :no-error-if-file-is-missing)

(server-start)
