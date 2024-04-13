(require 'window)

(defun my-move-buffer-other-window ()
  "Move the current buffer to the other window."
  (interactive)
  (let ((buffer (current-buffer)))
    (previous-buffer)
    (switch-to-buffer-other-window buffer)))

(provide 'my-window)
