;; -------------------------------
;; Emacs Init File - Clean & Modern
;; -------------------------------

;; setup # insert for mac emacs
(global-set-key (kbd "C-c <f2>") (lambda () (interactive) (insert "#")))


;; Maximise window on start
(push '(fullscreen . maximized) default-frame-alist)


;; Ensure org agenda is available and configured
(require 'org)
(require 'org-agenda)

(setq org-agenda-custom-commands
      '(("w" "Work Agenda"
         ((agenda "")
          (alltodo "")))
        ("p" "Personal Agenda"
         ((agenda "")
          (alltodo "" :tag "PERSONAL")))))


(global-set-key (kbd "C-c a") 'org-agenda)

;; -------------------------------
;; Package Manager Setup (MELPA)
;; -------------------------------
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure use-package is available
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; -------------------------------
;; Theme and Font
;; -------------------------------
;; Set default theme
(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t))

;; Set default font
(set-face-attribute 'default nil
                    :family "JetBrainsMono NF"
                    :height 140)  ;; Adjust as needed

;; Set fallback font for emoji
(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))

;; -------------------------------
;; Programming Mode Ligatures
;; -------------------------------
(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode
                          (list "==" "!=" "===" "&&" "||"
                                "->" "=>" "<=" ">=" "<->"
                                "::" ":::" "**" "++" "--"
                                ">>" "<<" "..." "?:" "::"))
  (global-ligature-mode t))

;; -------------------------------
;; Todo Sequence using emojis
;; -------------------------------
;;(setq org-todo-keywords
;;      '((sequence "BACKLOG(l!)" "TODO(t!)" "IN-PROGRESS(i!)" "BLOCKED(b@)" "|" "DONE(d!)" "NOT-ACCEPTED(n!)")))

;;(setq org-todo-keyword-faces
;;      '(("BACKLOG" . (:foreground "orange" :weight bold))
;;	("TODO" . (:foreground "orange" :weight bold))
;;        ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
;;        ("BLOCKED" . (:foreground "light blue" :weight bold))
;;        ("DONE" . (:foreground "green" :weight bold))
;;        ("NOT-ACCEPTED" . (:foreground "gray" :weight bold :strike-through t))))

;;(defun my/org-add-emoji-for-todo ()
;;  "Add or update emoji at the start of a heading based on the TODO keyword."
;;  (when (org-get-todo-state)
;;    (let* ((emoji-map '(("BACKLOG" . "❓")
;;			("TODO" . "🔧")
;;                        ("IN-PROGRESS" . "⏳")
;;                        ("BLOCKED" . "💤")
;;                        ("DONE" . "✔️")
;;                        ("NOT-ACCEPTED" . "❌")))
;;           (todo (org-get-todo-state))e
;;           (emoji (cdr (assoc todo emoji-map))))
;;      (when emoji
;;        (save-excursion
;;          (org-back-to-heading t)
;;          ;; Remove any existing emoji at start of heading text
;;          (when (looking-at "^\\*+ +\\S-+ +\\([[:nonascii:][:punct:][:symbol:]]+\\) ")
;;            (replace-match ""))
;;          ;; Insert the correct emoji
;;          (re-search-forward (regexp-quote todo))
;;          (insert " " emoji))))))

;;;; Hook it into Org whenever you change TODO state
;;(add-hook 'org-after-todo-state-change-hook #'my/org-add-emoji-for-todo)

;;(setq org-log-done 'time)
;;;; Enable persistent time tracking in Org
;;(setq org-clock-persist 'history)              ; Save clock history on exit
;;(org-clock-persistence-insinuate)              ; Restore clock data on startup
;;(setq org-clock-in-resume t)                   ; Resume clocking on same task
;;(setq org-clock-out-remove-zero-time-clocks t) ; Clean up 0:00 entries
;;(setq org-clock-report-include-clocking-task t); Show active task in reports

;; -------------------------------------
;; 1. Define custom TODO keywords
;; -------------------------------------
(setq org-todo-keywords
      '((sequence "BACKLOG(l!)" "TODO(t!)" "IN-PROGRESS(i!)" "BLOCKED(b@)"
                  "|" "DONE(d!)" "NOT-ACCEPTED(n!)")))

;; -------------------------------------
;; 2. Colour-code TODO keywords
;; -------------------------------------
(setq org-todo-keyword-faces
      '(("BACKLOG" . (:foreground "orange" :weight bold))
        ("TODO" . (:foreground "orange" :weight bold))
        ("IN-PROGRESS" . (:foreground "yellow" :weight bold))
        ("BLOCKED" . (:foreground "light blue" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("NOT-ACCEPTED" . (:foreground "gray" :weight bold :strike-through t))))

;; -------------------------------------
;; 3. Log timestamps when closing tasks
;; -------------------------------------
(setq org-log-done 'time)

;; -------------------------------------
;; 4. Optional: persistent time tracking
;; -------------------------------------
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-in-resume t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-report-include-clocking-task t)

;; -------------------------------------
;; 5. Enable org-superstar/org-modern (visual polish)
;; -------------------------------------
;;(use-package org-superstar
;;  :ensure t
;;  :hook (org-mode . org-superstar-mode)
;;  :config
;;  (setq org-superstar-headline-bullets-list '("◉" "○" "•" "▪" "▸"))
;;  (setq org-superstar-item-bullet-alist '((?- . ?•) (?+ . ?➤) (?* . ?✦)))
;;  (setq org-superstar-remove-leading-stars t)
;;  (setq org-hide-leading-stars nil)) ;; Leave this as nil so it works well with your coloured TODOs


;;(use-package org-modern
;;  :ensure t
;;  :hook (org-mode . org-modern-mode)
;;  :config
;;  (setq org-modern-todo-faces nil ; So it doesn’t override your custom colours
;;        org-modern-star '("◉" "○" "✿" "•")
;;        org-modern-checkbox '((?X . "✔") (?- . "–") (?\s . "☐")))) ; Pretty checkboxes

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :init
  ;; Define modern-style TODO keyword boxes using your existing colours
  (setq org-modern-todo-faces
        '(("BACKLOG" . (:inherit fixed-pitch :box t :weight bold
                       :foreground "#93a1a1" :background "#073642")) ; base1 on base02
          ("TODO" . (:inherit fixed-pitch :box t :weight bold
                       :foreground "#b58900" :background "#002b36"))   ; yellow on base03
          ("IN-PROGRESS" . (:inherit fixed-pitch :box t :weight bold
                       :foreground "#268bd2" :background "#002b36")) ; blue on base03
          ("BLOCKED" . (:inherit fixed-pitch :box t :weight bold
				 :foreground "#dc322f" :background "#002b36")) ; red on base03
          ("DONE" . (:inherit fixed-pitch :box t :weight bold
			      :foreground "#002b36" :background "#859900")) ; base03 on green
 
          ("NOT-ACCEPTED" . (:inherit fixed-pitch :box t :weight bold
				      :foreground "#586e75" :background "#002b36")))) ; base01 on base03
  ;; Enable pill-style tags
  (setq org-modern-tag t
	org-modern-label t
	org-modern-label-border 0.2    ; roundness of pill shape (0.0 - 0.5)
	org-modern-tag-truncate nil)    ; don't cut off tag text
  :config
  (setq org-modern-star '("◉" "○" "✿" "•")
        org-modern-checkbox '((?X . "✔") (?- . "–") (?\s . "☐"))))


;;-------------------------------------------
;; Org Capture Mode and templates
;;-------------------------------------------

(global-set-key (kbd "C-c c") 'org-capture)


(setq org-capture-templates
      `(
        ;; 🚧 Roadmap Task
        ("r" "Roadmap Task"
         entry (file+headline "~/Google Drive/My Drive/org/security/roadmap.org" "Roadmap Tasks")
         "* BACKLOG %^{Task Title}\n  :PROPERTIES:\n  :Created: %U\n  :Category: Roadmap\n  :JIRA_PROJECT: INFSEC\n  :JIRA_ISSUE_TYPE: Story\n    :END:\n%?"
         :empty-lines 1)

        ;; 📥 Quick Inbox Task
        ("t" "Quick Task to Inbox"
         entry (file+headline "~/Google Drive/My Drive/org/security/inbox.org" "Tasks")
         "* TODO %^{Task Title} %^g\n  :PROPERTIES:\n  :Created: %U\n  :Source: %^{Source|email|chat|meeting|call|ad-hoc}\n  :END:\n%?"
         :empty-lines 1)

        ;; 📞 Meeting Notes
        ("m" "Meeting or Call Notes"
         entry (file+datetree "~/Google Drive/My Drive/orgj/security/meetings.org")
         "* %U - %^{Title of discussion}\n:PROPERTIES:\n:Participants: %^{Who was present?}\n:Created: %U\n:END:\n\n%?"
         :empty-lines 1)
        

	("p" "Personal Task" entry
         (file+headline "~/org/tasks.org" "Inbox")
         "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  :PERSONAL:\n"
         :empty-lines 1)
       ))

;; Load optional local override file if it exists
(let ((local-config (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-config)
    (load-file local-config)))

;; Load org-jira and set the basic URL
(use-package org-jira
  :ensure t
  :config
  (setq jiralib-url "https://topcashback.atlassian.net"))


(setq org-jira-custom-jqls
  '(
    (:jql " project = INFSEC AND issuetype NOT IN (Epic) AND status NOT IN (Done, 'Not Accepted') ORDER BY created DESC"
          :filename "exported-from-jira-isec")
    ))

;; Define a reusable query for Jira
(defun my/org-jira-get-isec-base-issues ()
  (interactive)
  (org-jira-get-issues-from-custom-jql
   "project = INFSEC AND issuetype NOT IN (Epic) AND status NOT IN (Done, 'Not Accepted') ORDER BY created DESC"))

;; Create functions to only update and create items in jira that have the jira tag
(defun my/org-jira-update-tagged ()
  "Update only :jira:-tagged items in current buffer."
  (interactive)
  (org-map-entries #'org-jira-update-issue "+jira"))

(global-set-key (kbd "C-c j u") 'my/org-jira-update-tagged)

(defun my/org-jira-create-tagged ()
  "Create only :jira:-tagged items in current buffer."
  (interactive)
  (org-map-entries #'org-jira-create-issue "+jira"))

(global-set-key (kbd "C-c j u") 'my/org-jira-create-tagged)

(setq org-jira-todo-states
      '(( "Backlog" . "BACKLOG")
        ( "To Do" . "TODO")
        ( "In Progress" . "IN-PROGRESS")
        ( "Blocked" . "BLOCKED")
        ( "Done" . "DONE")
        ( "Cancelled" . "NOT-ACCEPTED")))


;; -------------------------------
;; Package Selections (for Custom)
;; -------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
