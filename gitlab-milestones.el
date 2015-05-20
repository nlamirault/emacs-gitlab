;;; gitlab-milestones.el --- Gitlab Milestones API

;; Copyright (C) 2015 Marcin Antczak <marcin.antczak@neutrico.eu>

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
;; https://github.com/gitlabhq/gitlabhq/blob/master/doc/api/milestones.md

;;; Code:

(require 's)

(require 'gitlab-utils)

(defun gitlab-list-milestones (project-id)
  "Get a list of project issues.

PROJECT-ID : The ID of a project"
  (perform-gitlab-request (s-concat "projects"
                                    (number-to-string project-id)
                                    "milestones"
                                    )
                          nil
                          200))

(defun gitlab--get-milestone-uri (project-id milestone-id)
  "Create milestone uri for PROJECT-ID identified by MILESTONE-ID."
  (s-concat "projects/"
            (number-to-string project-id)
            "/milestones/"
            (number-to-string milestone-id)))


(defun gitlab-get-milestone (project-id milestone-id)
  "Gets a single project milestone.

PROJECT-ID : The ID of a project
MILESTONE-ID : The ID of a project milestone"
  (perform-gitlab-request (gitlab--get-milestone-uri project-id milestone-id)
                          nil
                          200))

(defun gitlab-get-milestone-issues (project-id milestone-id)
  "Gets a single project milestone.

PROJECT-ID : The ID of a project
MILESTONE-ID : The ID of a project milestone"
  (perform-gitlab-request (s-concat
                           (gitlab--get-milestone-uri project-id milestone-id)
                           "issues")
                          nil
                          200))

(defun gitlab-create-milestone (project-id milestone-title)
  "Create a project milestone.

PROJECT-ID: The ID or NAMESPACE%2FPROJECT_NAME of a project
MILESTONE-TITLE: Title of milestone"
  (perform-gitlab-request ("POST"
                           (format "projects/%s/milestones" project-id)
                           "title=tescik"
                           201)))

(provide 'gitlab-milestones)
;;; gitlab-milestones.el ends here
