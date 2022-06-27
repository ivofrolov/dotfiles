(set-default 'inhibit-startup-screen t)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(horizontal-scroll-bar-mode 0)

(set-frame-font "Hack 13" nil t)

(setq modus-themes-syntax '(green-strings)
      modus-themes-subtle-line-numbers t)
(load-theme 'modus-operandi)

(set-default 'mode-line-position-column-format '(" C%C"))
(set-default 'mode-line-position-column-line-format '(" (%l,%C)"))
(global-display-line-numbers-mode)
(column-number-mode)

(set-default 'cursor-type 'bar)
(blink-cursor-mode 0)

(set-default 'indent-tabs-mode nil)
(set-default 'standard-indent 4)
(set-default 'tab-width 4)

(set-default 'visual-line-fringe-indicators '(nil right-curly-arrow))
(global-visual-line-mode 1)

(set-default 'apropos-do-all t)

(set-default 'load-prefer-newer t)

(set-default 'create-lockfiles nil)
(set-default 'make-backup-files nil)
(save-place-mode 1)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(set-default 'select-enable-clipboard nil)
(set-default 'select-enable-primary t)
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-x") 'clipboard-kill-region)
(global-set-key (kbd "s-v") 'clipboard-yank)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-S-k") 'kill-whole-line)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(use-package uniquify
  :init
  (set-default 'uniquify-buffer-name-style 'forward))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package dash-at-point
  :ensure t
  :bind (("C-c d" . dash-at-point)
         ("C-c e" . dash-at-point-with-docset)))

(use-package markdown-mode
  :ensure t)

(use-package elm-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

