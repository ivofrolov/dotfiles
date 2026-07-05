(require 'window)

(defun my-move-buffer-other-window (arg)
  "Move current buffer to the other window.
With \\[universal-argument] prefix stay in selected window."
  (interactive "P")
  (let ((buffer (current-buffer)))
    (previous-buffer)
    (switch-to-buffer-other-window buffer)
    (if arg (select-window (previous-window)))))

(provide 'my-window)
