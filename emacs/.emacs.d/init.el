;; -------------------------------
;; Emacs Init File - Clean & Modern
;; -------------------------------

;; Maximise window on start
(push '(fullscreen . maximized) default-frame-alist)

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
                    :height 110)  ;; Adjust as needed

;; Set fallback font for emoji
(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))

;; -------------------------------
;; Org Mode: Aesthetic Enhancements
;; -------------------------------
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿")
        org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))  ;; + - *
        org-modern-checkbox '((?X . "☒") (?\[ . "☐") (?\] . "☑"))
        org-modern-block-fringe nil
        org-modern-table nil
        org-hide-emphasis-markers t))

;; Prettify symbols in Org mode
(defun my/org-prettify-symbols ()
  "Prettify Org symbols using safe character sequences."
  (setq prettify-symbols-alist
        (list
         (cons "[ ]" "☐")
         (cons "[X]" "☑")
         (cons "[-]" "❍")
         (cons "#+BEGIN_SRC" "⟦")
         (cons "#+END_SRC"   "⟧")
         (cons "#+begin_src" "⟦")
         (cons "#+end_src"   "⟧")
         (cons "->" "→")
         (cons "=>" "⇒")))
  (prettify-symbols-mode 1))

(add-hook 'org-mode-hook #'my/org-prettify-symbols)

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
(setq org-todo-keywords
      '((sequence "TODO(t)" "INPROG(i)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "orange" :weight bold))
        ("INPROG" . (:foreground "yellow" :weight bold))
        ("WAIT" . (:foreground "light blue" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("CANCELLED" . (:foreground "gray" :weight bold :strike-through t))))

(defun my/org-add-emoji-for-todo ()
  "Add or update emoji at the start of a heading based on the TODO keyword."
  (when (org-get-todo-state)
    (let* ((emoji-map '(("TODO" . "🔧")
                        ("INPROG" . "⏳")
                        ("WAIT" . "💤")
                        ("DONE" . "✔️")
                        ("CANCELLED" . "❌")))
           (todo (org-get-todo-state))e
           (emoji (cdr (assoc todo emoji-map))))
      (when emoji
        (save-excursion
          (org-back-to-heading t)
          ;; Remove any existing emoji at start of heading text
          (when (looking-at "^\\*+ +\\S-+ +\\([[:nonascii:][:punct:][:symbol:]]+\\) ")
            (replace-match ""))
          ;; Insert the correct emoji
          (re-search-forward (regexp-quote todo))
          (insert " " emoji))))))

;; Hook it into Org whenever you change TODO state
(add-hook 'org-after-todo-state-change-hook #'my/org-add-emoji-for-todo)


;; -------------------------------
;; Package Selections (for Custom)
;; -------------------------------
(custom-set-variables
 '(package-selected-packages
   '(ligature org-modern solarized-theme use-package)))
(custom-set-faces
 ;; Keep this here in case custom faces are used
 )
