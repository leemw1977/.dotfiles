;; NOTE: This file expects these vars to be defined before loading.
;; You can set this in ~/.emacs.d/my-machine.el to override the default.
(defvar my/org-root-path
  "/mnt/g/My Drive/org/security"
  "Root directory for Org files and Jira data on this machine.")

(defvar my/default-font-size 150
  "Default font size (in 1/10 pt units) for the current machine.")

;;; local.el --- Machine-specific Emacs config using org-root-path

;; This file contains shared local config (paths, layout) for Org and Org-Jira.
;; It expects `my/org-root-path` to be optionally set before loading.
;; If not set, a default is used below.

(defvar my/org-root-path
  "~/Google Drive/My Drive/org/security"
  "Root directory for Org-Jira and agenda files on this machine.")

;; Set agenda and org-jira directories
(setq org-agenda-files (list my/org-root-path))
(setq org-jira-working-dir my/org-root-path)

;; Optional: define work layout using root path
(defun my-work-layout ()
  "Open key Org files in a three-pane layout."
  (interactive)
  (delete-other-windows)
  (let ((file-a (expand-file-name "INFSEC.org" my/org-root-path))
        (file-b (expand-file-name "inbox.org" my/org-root-path))
        (file-c (expand-file-name "meetings.org" my/org-root-path)))
    (find-file file-a)))
;;    (let ((right (split-window-right)))
;;      (select-window right)
;;      (find-file file-b)
;;      (let ((bottom (split-window-below)))
;;        (select-window bottom)
;;        (find-file file-c)))))

;; Optional: bind layout to F9
(global-set-key (kbd "<f9>") #'my-work-layout)

(provide 'local)
;;; local.el ends here



;; (setq org-agenda-files '("/mnt/g/My Drive/org/security"))

;; (setq org-jira-working-dir "/mnt/g/My Drive/org/security")

;; (defun my-work-layout ()
;;   "Open three files in a specific window layout."
;;   (interactive)
;;   (delete-other-windows) ;; start with a clean layout
;;   (let ((file-a "/mnt/g/My Drive/org/security/INFSEC.org")
;;         (file-b "/mnt/g/My Drive/org/security/inbox.org")
;;         (file-c "/mnt/g/My Drive/org/security/meetings.org"))
;;     ;; Open file A in the main window
;;     (find-file file-a)
;;     ;; Split right for B and C
;;     (let ((right (split-window-right)))
;;       (select-window right)
;;       (find-file file-b)
;;       ;; Split B vertically for C
;;       (let ((bottom (split-window-below)))
;;         (select-window bottom)
;;         (find-file file-c)))))

;; ;; Optional: bind it to a key, e.g., F9
;; (global-set-key (kbd "<f9>") #'my-work-layout)
