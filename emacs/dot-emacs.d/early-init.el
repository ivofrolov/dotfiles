(setq inhibit-splash-screen t
      inhibit-startup-screen t)

(setq frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(tool-bar-mode 0)
(scroll-bar-mode 0)
(horizontal-scroll-bar-mode 0)

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
