(setq gtd-buffer-list '("gtd.org" "gtd.org_archive" "notes.org" "notes.org_archive"))
(defun ak/delete-buffer-list (buffer)
  "Delete buffer if exists"
  (when (get-buffer buffer)
    (kill-buffer buffer)))
(defun ak/save-exit-buffer-list ()
  "save all buffers in list then close them. Used for keeping gtd from conflicting on multiple machines"
  (interactive)
  (save-some-buffers gtd-buffer-list)
  (mapcar #'ak/delete-buffer-list gtd-buffer-list))
