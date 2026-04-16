;;; init.el --- Minimal sensible starting point for Org-based workflow -*- lexical-binding: t; -*-

(defvar my/org-base-directory "~/org"
  "Base directory for Org files.")

(let ((local-config (expand-file-name "my-machine.el" user-emacs-directory)))
  (when (file-exists-p local-config)
    (load-file local-config)))

(unless (file-directory-p my/org-base-directory)
  (warn "Org directory does not exist: %s" my/org-base-directory))
  

(setq org-directory my/org-base-directory)

(defvar org-inbox-file (expand-file-name "inbox.org" org-directory)
  "Primary Org inbox file.")

(defvar org-main-file (expand-file-name "main.org" org-directory)
  "Primary Org task file.")
;; --------------------------------------------------
;; Basic editor behaviour
;; --------------------------------------------------

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(save-place-mode 1)
(recentf-mode 1)

(setq make-backup-files t)
(setq auto-save-default t)
(setq create-lockfiles nil)

;; Prefer y/n over yes/no10
(fset 'yes-or-no-p 'y-or-n-p)

;; Use spaces by default
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; --------------------------------------------------
;; Org base settings
;; --------------------------------------------------

(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-clock)

(setq org-directory my/org-base-directory)
(setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

(setq org-agenda-files
      (list (expand-file-name "inbox.org" org-directory)
            (expand-file-name "main.org" org-directory)))

;; Log when tasks are marked done
(setq org-log-done 'time)

;; Keep indentation tidy in Org buffers
(setq org-startup-indented t)
(setq org-hide-emphasis-markers t)

;; --------------------------------------------------
;; TODO workflow
;; --------------------------------------------------

(setq org-todo-keywords
      '((sequence "TODO(t)"
                  "NEXT(n)"
                  "IN-PROGRESS(i)"
                  "WAITING(w)"
                  "BLOCKED(b)"
                  "|"
                  "DONE(d)"
                  "CANCELLED(c)")))

(setq org-todo-keyword-faces
      '(("NEXT" . "purple")
        ("IN-PROGRESS" . "blue")
        ("WAITING" . "dark goldenrod")
        ("BLOCKED" . "red")
        ("CANCELLED" . "grey")))

;; --------------------------------------------------
;; Capture templates
;; --------------------------------------------------

(setq org-capture-templates
      `(("t" "Task to inbox" entry
         (file+headline ,org-inbox-file "Inbox")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")

        ("n" "Note to inbox" entry
         (file+headline ,org-inbox-file "Inbox")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")))

;; --------------------------------------------------
;; Refile targets
;; --------------------------------------------------

(setq org-refile-targets
      `((,org-main-file :maxlevel . 2)
        (,org-inbox-file :maxlevel . 2)))

(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)

;; --------------------------------------------------
;; Clocking
;; --------------------------------------------------

;; Persist clocks across Emacs restarts
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Resume clock when clocking into task
(setq org-clock-in-resume t)

;; Remove zero-length clocks
(setq org-clock-out-remove-zero-time-clocks t)

;; Save clock data into LOGBOOK drawer
(setq org-clock-into-drawer "LOGBOOK")

;; If already clocking something, prompt when switching
(setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
(setq org-clock-mode-line-total 'today)

;; --------------------------------------------------
;; Agenda views
;; --------------------------------------------------

(setq org-agenda-custom-commands
      '(("d" "Daily Dashboard"
         ((agenda "" ((org-agenda-span 1)
                      (org-deadline-warning-days 7)))
          (todo "IN-PROGRESS")
          (todo "NEXT")
          (todo "WAITING")))))


;; Built-in effort estimates
(setq org-global-properties
      '(("Effort_ALL" . "0:10 0:15 0:30 1:00 2:00 3:00 4:00 8:00")))

(setq org-columns-default-format
      "%50ITEM(Task) %10TODO(Status) %10Effort(Estimate){:} %10CLOCKSUM(Time Spent)")

(setq org-agenda-columns-add-appointments-to-effort-sum t)

;; Skip done tasks in agenda where sensible
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-start-on-weekday nil)

;; --------------------------------------------------
;; Convenience keybindings
;; --------------------------------------------------

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)

;; Optional personal shortcuts
(global-set-key (kbd "<f12>") (lambda ()
                                (interactive)
                                (org-agenda nil "d")))

(global-set-key (kbd "<f11>") #'org-clock-in)
(global-set-key (kbd "S-<f11>") #'org-clock-out)
(global-set-key (kbd "C-<f11>") #'org-clock-goto)

(global-set-key (kbd "<f10>") #'org-archive-subtree)
;; --------------------------------------------------
;; Auto-open dashboard on startup
;; --------------------------------------------------

(defun my/open-startup-dashboard ()
  "Open my daily Org agenda dashboard."
  (when (file-directory-p org-directory)
    (org-agenda nil "d")))

(add-hook 'emacs-startup-hook #'my/open-startup-dashboard)

;; --------------------------------------------------
;; Ensure basic files exist
;; --------------------------------------------------

(defun my/ensure-org-file (file title heading)
  "Create FILE with TITLE and top HEADING if it does not exist."
  (unless (file-exists-p file)
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert "#+TITLE: " title "\n\n")
      (insert "* " heading "\n"))))

(my/ensure-org-file org-inbox-file "Inbox" "Inbox")
(my/ensure-org-file org-main-file  "Main"  "ACTIVE TASKS")

;;; init.el ends here
