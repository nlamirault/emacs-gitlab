;;; gitlab-test-utils.el --- Utils for unit tests for Gitlab

;; Copyright (C) Nicolas Lamirault <nicolas.lamirault@gmail.com>

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

;;; Code:

(require 'gitlab)

(defmacro with-gitlab-session (&rest body)
  `(progn
     (gitlab-login)
     ,@body
     (setq gitlab-token-id nil)))


(defun gitlab-project-id ()
  (getenv "GITLAB_PROJECT_ID"))

(defun gitlab-project-name ()
  (getenv "GITLAB_PROJECT_NAME"))

(defun gitlab-project-description ()
  (getenv "GITLAB_PROJECT_DESC"))


(provide 'gitlab-test-utils)
;;; gitlab-test-utils.el ends here
