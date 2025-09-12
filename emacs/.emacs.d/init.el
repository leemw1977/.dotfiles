;; -*- lexical-binding: t; -*-
;; (add-to-list 'load-path "~/src/leemw1977/org-ticketflow")

;; -------------------------------
;; Emacs Init File - Clean & Modern
;; -------------------------------

(global-set-key (kbd "C-c r")
                (lambda () (interactive)
                  (load-file user-init-file)
                  (message "🔄 Reloaded init.el")))

;; Set auth sources
(setq auth-sources '("~/.authinfo.gpg"))



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



;; -----------------------------------
;; Configuration and local overrides
;; -----------------------------------
;; Set default them
;; Load machine-specific config if it exists
(let ((machine-specific (expand-file-name "my-machine.el" user-emacs-directory)))
  (when (file-exists-p machine-specific)
    (load machine-specific)
    (message "my-machine.el loaded from %s" user-emacs-directory)))

;; Load  optional local override file if it exists
(let ((local-config (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-config)
    (load-file local-config)
    (message "local.el load from %s" user-emacs-directory)))

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
                    :height my/default-font-size)  ;; Adjust as needed

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



;; -------------------------------------
;; 1. Define custom TODO keywords
;; -------------------------------------
(setq org-todo-keywords
      '((sequence "BACKLOG" "TODO(t)" "IN-PROGRESS(i)" "BLOCKED(b)" "|" "DONE(d)" "NOT-ACCEPTED(c)")))

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
         entry (file+headline ,(expand-file-name "roadmap.org" my/org-root-path) "Roadmap Tasks")
         "* BACKLOG %^{Task Title}\n  :PROPERTIES:\n  :Created: %U\n  :Category: Roadmap\n  :JIRA_PROJECT: INFSEC\n  :JIRA_ISSUE_TYPE: Story\n  :END:\n%?"
         :empty-lines 1)

        ;; 📥 Quick Inbox Task
        ("t" "Quick Task to Inbox"
         entry (file+headline ,(expand-file-name "inbox.org" my/org-root-path) "Tasks")
         "* TODO %^{Task Title} %^g\n  :PROPERTIES:\n  :Created: %U\n  :Source: %^{Source|email|chat|meeting|call|ad-hoc}\n  :END:\n%?"
         :empty-lines 1)

        ;; 📞 Meeting Notes
        ("m" "Meeting or Call Notes"
         entry (file+datetree ,(expand-file-name "meetings.org" my/org-root-path))
         "* %U - %^{Title of discussion}\n:PROPERTIES:\n:Participants: %^{Who was present?}\n:Created: %U\n:END:\n\n%?"
         :empty-lines 1)

        ;; 🏠 Personal Task (unchanged path, adjust if desired)
        ("p" "Personal Task"
         entry (file+headline "~/org/tasks.org" "Inbox")
         "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  :PERSONAL:\n"
         :empty-lines 1)
        ))

;; (use-package org-ticketflow
;;  :load-path "~/src/leemw1977/org-ticketflow")

;; ;; Load org-jira and set the basic URL
;; (use-package org-jira
;;   :load-path "~/src/leemw1977/org-jira"
;;   :after org
;;   :init
;;   (setq jiralib-url "https://topcashback.atlassian.net"
;;         org-jira-todo-states
;;         '(( "Backlog" . "BACKLOG")
;;         ( "To Do" . "TODO")
;;         ( "In Progress" .  "IN-PROGRESS")
;;         ( "Blocked" . "BLOCKED")
;;         ( "Done" . "DONE")
;;         ( "Cancelled" . "NOT-ACCEPTED")))
;;   :ensure t)


;; (setq org-jira-custom-jqls
;;   '(
;;     (:jql " project = INFSEC AND issuetype NOT IN (Epic) AND status NOT IN (Done, 'Not Accepted') ORDER BY created DESC"
;;           :filename "exported-from-jira-isec")
;;     ))


;; (setq org-jira-todo-states
;;       '(( "Backlog" . "BACKLOG")
;;         ( "To Do" . "TODO")
;;         ( "In Progress" .  "IN-PROGRESS")
;;         ( "Blocked" . "BLOCKED")
;;         ( "Done" . "DONE")
;;         ( "Cancelled" . "NOT-ACCEPTED")))

;; -------------------------------
;; Package Selections (for Custom)
;; -------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ligature org-jira org-modern org-superstar plz solarized-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; (add-to-list 'load-path "~/.emacs.d/org-jira-extensions")
;; (require 'org-jira-extensions)


;; -------------------------------
;; Jira Link Insertion
;; -------------------------------
;;;; --- Jira links + optional target labels ---  (paste into init.el)

(defgroup lw/jira-link nil
  "Insert and follow Org links to Jira tickets, with optional Org targets."
  :group 'org)

(defcustom lw/jira-base-url "https://topcashback.atlassian.net"
  "Base URL of your Jira instance, no trailing slash."
  :type 'string
  :group 'lw/jira-link)

(defcustom lw/jira-insert-target-default 'always
  "Whether to add a dedicated Org target (<<KEY>>) after inserting the link.
Possible values:
- 'never  : never add
- 'ask    : prompt each time
- 'always : always add
A universal prefix arg (C-u) to `lw/jira-insert-link` forces adding the target."
  :type '(choice (const :tag "Never" never)
                 (const :tag "Ask" ask)
                 (const :tag "Always" always))
  :group 'lw/jira-link)

(defcustom lw/jira-target-placement 'after-link
  "Where to place the target when added.
- 'after-link : immediately after the inserted link (with a space)
- 'eol        : at end of the current line (prepend a space if needed)
- 'eob        : at end of buffer on a new line"
  :type '(choice (const after-link) (const eol) (const eob))
  :group 'lw/jira-link)

(defcustom lw/jira-target-transform #'upcase
  "Function applied to the Jira key before building the target text.
Use `identity` if you don’t want to change case."
  :type 'function
  :group 'lw/jira-link)

(defun lw/jira--issue-url (key)
  "Return the full HTTPS URL for Jira issue KEY."
  (unless (and (stringp lw/jira-base-url)
               (string-match-p "^https?://" lw/jira-base-url))
    (user-error "Set lw/jira-base-url to your Jira site first"))
  (format "%s/browse/%s" lw/jira-base-url key))

(with-eval-after-load 'org
  (org-link-set-parameters
   "jira"
   :follow (lambda (key) (browse-url (lw/jira--issue-url key)))
   :export (lambda (key desc backend)
             (let* ((url (lw/jira--issue-url key))
                    (label (or desc key)))
               (pcase backend
                 ((or 'html 'hugo 'ox-hugo) (format "<a href=\"%s\">%s</a>" url label))
                 ('md                        (format "[%s](%s)" label url))
                 (_                          (format "%s (%s)" label url)))))))

(defun lw/jira-infer-key-at-point ()
  "Best-effort guess of a Jira issue key at point (e.g., ABC-123)."
  (let ((case-fold-case nil))
    (save-excursion
      (let ((sym (thing-at-point 'symbol t)))
        (when (and sym (string-match-p "^[A-Z][A-Z0-9]+-[0-9]+$" sym))
          sym)))))

(defun lw/jira--maybe-insert-target (key forced)
  "Insert a dedicated target based on KEY according to user prefs.
If FORCED is non-nil, insert regardless of `lw/jira-insert-target-default`."
  (let* ((decision (cond
                    (forced 'always)
                    (t lw/jira-insert-target-default)))
         (do-it (pcase decision
                  ('never  nil)
                  ('always t)
                  ('ask    (y-or-n-p (format "Also insert target <<%s>>? "
                                             (funcall lw/jira-target-transform key))))
                  (_ nil))))
    (when do-it
      (let ((target (format "<<%s>>" (funcall lw/jira-target-transform key))))
        (pcase lw/jira-target-placement
          ('after-link (insert " " target))
          ('eol        (save-excursion
                         (end-of-line)
                         (unless (bolp) (insert " "))
                         (insert target)))
          ('eob        (save-excursion
                         (goto-char (point-max))
                         (unless (bolp) (insert "\n"))
                         (insert target))))
        t))))

;;;###autoload
(defun lw/jira-insert-link (&optional key)
  "Insert an Org link to a Jira issue, and optionally append a target <<KEY>>.
With a universal prefix argument (C-u), force adding the target regardless of
`lw/jira-insert-target-default`."
  (interactive)
  (let* ((forced (and current-prefix-arg t))
         (default-key (lw/jira-infer-key-at-point))
         (key (or key (read-string (if default-key
                                       (format "Jira issue key (default %s): " default-key)
                                     "Jira issue key: ")
                                   nil nil default-key)))
         (desc (read-string "Description (blank = use key): " nil nil key)))
    (insert (format "[[jira:%s][%s]]" key (if (string-empty-p desc) key desc)))
    (lw/jira--maybe-insert-target key forced)))

;; Optional: bind in Org buffers
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c j") #'lw/jira-insert-link))

(defun clear-messages-buffer ()
  "Forcefully erase the *Messages* buffer."
  (interactive)
  (let ((messages-buffer (get-buffer "*Messages*")))
    (when messages-buffer
      (with-current-buffer messages-buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (message "✅ Cleared *Messages* buffer."))))))
