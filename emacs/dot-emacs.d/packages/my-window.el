(require 'window)

(defun my-move-buffer-other-window ()
  "Move the current buffer to the other window."
  (interactive)
  (let ((buffer (current-buffer)))
    (previous-buffer)
    (other-window 1)
    (set-window-buffer (selected-window) buffer)))

(provide 'my-window)
