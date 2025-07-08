;; -*- lexical-binding: t; -*-

;;; org-jira-extensions --- Jira API integration for Emacs

;; Author: Lee Williams <lee.williams@topcashback.co.uk>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (request "0.3.0"))
;; Keywords: tools jira api
;; URL: https://github.com/yourusername/my-jira

;;; Commentary:

;; Simple Jira API integration for querying issue status, summary, and assignee.

;;; Code:


(require 'request)
(require 'auth-source)
(require 'cl-lib)


;; Define a reusable query for Jira
(setq org-jira-custom-jqls
  '(
    (:jql " project = INFSEC AND issuetype NOT IN (Epic,'GTM Change Request') AND status NOT IN ('Not Accepted') ORDER BY created DESC "
          :filename "INFSEC")
   ))



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

(global-set-key (kbd "C-c j c") 'my/org-jira-create-tagged)



(defvar my/jira-custom-fields
  '(("Start date" . "customfield_10419"))
  "Mapping of Jira custom field display names to their field IDs.")


(defun  my/jira-auth-header ()
  "Return a Basic Auth header using credentials from .authinfo.gpg."
  (let* ((creds (car (auth-source-search
                      :host "topcashback.atlassian.net"
                      :require '(:user :secret))))
         (user (plist-get creds :user))
         (secret (if (functionp (plist-get creds :secret))
                     (funcall (plist-get creds :secret))
                   (plist-get creds :secret))))
    (format "Basic %s"
            (base64-encode-string (concat user ":" secret) t))))

(defun my/jira-get-issue ()
  "Prompt for a Jira issue key and fetch its summary asynchronously."
  (interactive)
  (let* ((issue-key (read-string "Enter Jira issue key (e.g. INFSEC-374): "))
         (url (format "https://topcashback.atlassian.net/rest/api/2/issue/%s" issue-key))
         (auth (my/jira-auth-header)))
    (request
     url
     :type "GET"
     :headers `(("Authorization" . ,auth)
                ("Content-Type" . "application/json"))
     :parser 'json-read
     :success
     (lambda (&rest args)
       (let* ((data (plist-get args :data))
              (fields (alist-get 'fields data))
              (summary (alist-get 'summary fields))
              (status (alist-get 'name (alist-get 'status fields)))
              (assignee (alist-get 'displayName (alist-get 'assignee fields))))
         (message "[%s] %s\nStatus: %s | Assignee: %s"
                  issue-key summary status (or assignee "Unassigned"))))
     :error
     (lambda (&rest args)
       (let ((status (request-response-status-code (plist-get args :response))))
         (message "Jira call failed for %s: %s"
                  issue-key status))))))


(defun my/jira-update-start-date (issue-key new-date)
  "Update the 'Start date' custom field on ISSUE-KEY to NEW-DATE (YYYY-MM-DD)."
  (interactive
   (list
    (read-string "Jira issue key (e.g. INFSEC-374): ")
    (read-string "Start date (YYYY-MM-DD): ")))
  (let* ((field-id (alist-get "Start date" my/jira-custom-fields nil nil #'string=)))
    (if (not field-id)
        (message "❌ No field ID found for 'Start date'.")
      (let* ((url (format "https://topcashback.atlassian.net/rest/api/2/issue/%s" issue-key))
             (auth (my/jira-auth-header))
             (data `(("fields" . (,(cons field-id new-date)))))
             (json-payload (json-encode data)))
        (message "Sending to Jira: %s" json-payload)
        (request
         url
         :type "PUT"
         :headers `(("Authorization" . ,auth)
                    ("Content-Type" . "application/json"))
         :data json-payload
         :parser 'buffer-string
         :success (lambda (&rest _) (message "✅ Updated Start date for %s" issue-key))
         :error (lambda (&rest args)
                  (let ((resp (plist-get args :response)))
                    (message "❌ Error %s for %s: %s"
                             (request-response-status-code resp)
                             issue-key
                             (plist-get args :data)))))))))


(defun my/org-jira-get-issue-key ()
  "Return the Jira issue key from the current heading or its ancestors via :ID: property."
  (interactive)
  (save-excursion
    (let ((found nil))
      (while (and (not found)
                  (or (org-at-heading-p)
                      (org-back-to-heading t)))
        (setq found (org-entry-get (point) "ID"))
        (unless found
          (if (not (org-up-heading-safe))
              (setq found 'not-found))))
      (cond
       ((stringp found)
        (message "✅ Found Jira issue key (from :ID:): %s" found)
        found)
       ((eq found 'not-found)
        (message "❌ No :ID: property found in this entry or any ancestors.")
        nil)
       (t
        (message "⚠️ Unexpected condition while finding Jira ID.")
        nil)))))




(provide 'org-jira-extensions)
;;; org-jira-extensions ends here
