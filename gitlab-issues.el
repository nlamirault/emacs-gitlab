;;; gitlab-issues.el --- Issues API

;; Copyright (C) 2014 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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
    (perform-gitlab-request "issues" params 200)))


(defun gitlab--get-issue-uri (project-id issue-id)
  (s-concat "projects/"
            (number-to-string project-id)
            "/issues/"
            (number-to-string issue-id)))

(defun gitlab-list-project-issues (project-id)
  "Get a list of project issues.

PROJECT-ID : The ID of a project"
  (perform-gitlab-request (s-concat "projects/"
                                    (number-to-string project-id)
                                    "/issues")
                          nil
                          200))

(defun gitlab-get-issue (project-id issue-id)
  "Gets a single project issue.

PROJECT-ID : The ID of a project
ISSUE-ID : The ID of a project issue"
  (perform-gitlab-request (gitlab--get-issue-uri project-id issue-id)
                          nil
                          200))


(provide 'gitlab-issues)
;;; gitlab-issues.el ends here
