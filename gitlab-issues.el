;;; gitlab-issues.el --- Issues API

;; Copyright (C) 2014, 2015 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; See API doc :
;; https://github.com/gitlabhq/gitlabhq/blob/master/doc/api/issues.md


;;; Code:

(require 's)

(require 'gitlab-utils)


(defun gitlab-list-issues ()
  "Get all issues created by authenticated user.
STATE Return all issues or just those that are opened or closed
LABELS - Comma-separated list of label names"
  (let ((params '()))
    ;; (when state
    ;;   (add-to-list params (cons "state" state)))
    ;; (when labels
    ;;   (add-to-list params (cons "labels" labels)))
    (perform-gitlab-request "GET" "issues" params 200)))


(defun gitlab--get-issue-uri (project-id issue-id)
  "Retrieve URI to retrieve an issue.
PROJECT-ID : The ID of a project
ISSUE-ID : The ID of a project issue"
  (s-concat "projects/"
            (url-hexify-string
             (format "%s" project-id))
            "/issues/"
            issue-id))

(defun gitlab-list-project-issues (project-id)
  "Get a list of project issues.

PROJECT-ID : The ID of a project"
  (perform-gitlab-request "GET"
                          (s-concat "projects/"
                                    (url-hexify-string
                                     (format "%s" project-id))
                                    "/issues")
                          nil
                          200))

(defun gitlab-get-issue (project-id issue-id)
  "Gets a single project issue.

PROJECT-ID : The ID of a project
ISSUE-ID : The ID of a project issue"
  (perform-gitlab-request "GET"
                          (gitlab--get-issue-uri
                           (url-hexify-string
                            (format "%s" project-id))
                           (format "%s" issue-id))
                          nil
                          200))

(defun gitlab-create-issue (project-id title &optional description assignee milestone labels)
  "Create a project issue.

PROJECT-ID the ID or NAMESPACE%2FPROJECT_NAME of a project
TITLE issue title
DESCRIPTION issue description
ASSIGNEE assignee ID
MILESTONE milestone ID
LABELS comma-separated list label names"
  (lwarn '(gitlab) :debug "Create ISSUE in project: %s" project-id)
  (perform-gitlab-request "POST"
                          (format "projects/%s/issues"
                                   (url-hexify-string
                                    (format "%s" project-id)))
                          (format "title=%s%s"
                                  title
                                  (concat
                                   (when description
                                    (format "&description=%s" description))
                                  (when assignee
                                    (format "&assignee_id=%s" assignee))
                                  (when milestone
                                    (format "&milestone_id=%s" milestone))
                                  (when labels
                                    (format "&labels=%s" labels))
                                  ))
                           201))

(defun gitlab-edit-issue (project-id issue-id &optional title description assignee-id milestone-id labels state-event)
  "Create a project issue.

PROJECT-ID the ID or NAMESPACE%2FPROJECT_NAME of a project
TITLE issue title
DESCRIPTION issue description
ASSIGNEE assignee ID
MILESTONE milestone ID
LABELS comma-separated list label names"
  (lwarn '(gitlab) :debug "UPDATE ISSUE in project: %s\n" project-id)
  (perform-gitlab-request "PUT"
                          (format "projects/%s/issues/%s"
                                   (url-hexify-string
                                    (format "%s" project-id))
                                   issue-id)

                          (format "%s"
                                  (concat
                                   (when title
                                     (format "&title=%s" title))
                                   (when description
                                     (format "&description=%s" description))
                                   (when assignee-id
                                     (format "&assignee_id=%s" assignee-id))
                                   (when milestone-id
                                     (format "&milestone_id=%s" milestone-id))
                                   (when labels
                                     (format "&labels=%s" labels))
                                   (when state-event
                                     (format "&state_event=%s" state-event))))
                          200))


(provide 'gitlab-issues)
;;; gitlab-issues.el ends here
