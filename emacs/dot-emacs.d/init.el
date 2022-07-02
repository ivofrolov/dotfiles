(add-to-list 'package-archives '("melpa" . "https://stable.melpa.org/packages/") t)

(eval-when-compile
  (unless (package-installed-p 'use-package)
    (require 'package)
    (package-install 'use-package))
  (require 'use-package))

(set-frame-font "Hack 13" nil t)
(setq modus-themes-region '(no-extend))
(load-theme 'modus-operandi)
(global-set-key (kbd "<f5>") 'modus-themes-toggle)

(setq-default cursor-type 'bar)
(blink-cursor-mode 0)

(setq mode-line-position-column-format '(" C%C")
      mode-line-position-column-line-format '(" (%l,%C)")
      visual-line-fringe-indicators '(nil right-curly-arrow))
(global-display-line-numbers-mode)
(column-number-mode)

(setq-default indent-tabs-mode nil
              standard-indent 4
              tab-width 4)

(global-visual-line-mode 1)

(setq select-enable-clipboard nil
      select-enable-primary t)
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-x") 'clipboard-kill-region)
(global-set-key (kbd "s-v") 'clipboard-yank)

(setq create-lockfiles nil
      make-backup-files nil)
(save-place-mode 1)

(setq read-buffer-completion-ignore-case t
      apropos-do-all t)

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-S-k") 'kill-whole-line)

(use-package uniquify
  :init
  (set-default 'uniquify-buffer-name-style 'forward))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (setq orderless-matching-styles '(orderless-initialism orderless-regexp)
        completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package markdown-mode
  :ensure t)

(use-package elm-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
