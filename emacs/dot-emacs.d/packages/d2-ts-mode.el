;;; d2-ts-mode.el --- Major mode for D2: Declarative Diagramming -*- lexical-binding: t -*-

;; See <https://d2lang.com/>

(require 'treesit)
(require 'reformatter)

;;;###autoload (autoload 'd2-format-buffer "current-file" nil t)
;;;###autoload (autoload 'd2-format-region "current-file" nil t)
;;;###autoload (autoload 'd2-format-on-save-mode "current-file" nil t)
(reformatter-define d2-format
  :program "d2"
  ;; though d2 supports working with standard input/output, it doesn't
  ;; write anything if file is already formatted
  :args (list "fmt" input-file)
  :stdin nil
  :stdout nil)

(defvar d2--treesit-language-grammar
  "https://github.com/ravsii/tree-sitter-d2.git")

(defvar d2-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for D2 files.")

(defvar d2--treesit-font-lock-settings
  (treesit-font-lock-rules
   :language 'd2
   :feature 'comment
   '((line_comment) @font-lock-comment-face
     (block_comment) @font-lock-comment-face))
  "Tree-sitter font-lock settings.")

(defvar d2--treesit-indent-rules
  `((d2
     ((node-is "}") parent-bol 0)
     ((parent-is "block") parent-bol 2)))
  "Tree-sitter indent rules")

(defvar d2-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") 'd2-format-buffer)
    map)
  "Keymap for `d2-ts-mode'.")

(defun d2--ts-mode-setup ()
  (setq-local treesit-font-lock-settings d2--treesit-font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment)))
  (setq-local treesit-simple-indent-rules d2--treesit-indent-rules)
  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-ts-mode))

;;;###autoload
(define-derived-mode d2-ts-mode prog-mode "D2"
  "Major mode for editing D2 files, using tree-sitter library.

\\{d2-ts-mode-map}"
  :syntax-table d2-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")

  (setq-local treesit-language-source-alist
              (list (cons 'd2 (list d2--treesit-language-grammar))))

  (when (treesit-ready-p 'd2)
    (treesit-parser-create 'd2)
    (d2--ts-mode-setup)))

(provide 'd2-ts-mode)
