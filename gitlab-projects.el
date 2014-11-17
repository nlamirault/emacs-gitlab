;;; gitlab-projects.el --- Projects API

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
;; https://github.com/gitlabhq/gitlabhq/blob/master/doc/api/projects.md


;;; Code:

(require 's)

(require 'gitlab-utils)


(defun gitlab-list-projects ()
  "Get a list of projects accessible by the authenticated user."
  (perform-gitlab-request "projects" nil 200))


(defun gitlab-list-owned-projects ()
  "Get a list of projects which are owned by the authenticated user."
  (perform-gitlab-request "projects/owned" nil 200))


(defun gitlab-get-project (project-id)
  "Get a specific project, identified by PROJECT-ID."
  (perform-gitlab-request (format "projects/%s" project-id)
                          nil
                          200))

(defun gitlab-search-projects (name)
  "Search for projects by name which are accessible to the authenticated user.
NAME is a string contained in the project name."
  (perform-gitlab-request (s-concat "projects/search/" name)
                          nil
                          200))


(defun gitlab-list-project-members (project-id)
  "Get a list of a project's team members.
PROJECT-ID is The ID or NAMESPACE/PROJECT_NAME of a project."
  (perform-gitlab-request (format "projects/%s/members" project-id)
                          nil
                          200))


(defun gitlab-list-project-events (project-id)
  "Get the events for the specified project, identified by PROJECT-ID.
Sorted from newest to latest."
  (perform-gitlab-request (format "projects/%s/events" project-id)
                          nil
                          200))

(provide 'gitlab-projects)
;;; gitlab-projects.el ends here
