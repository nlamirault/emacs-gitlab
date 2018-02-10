;;; gitlab-projects-test.el --- Tests for Gitlab projects API

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

(require 's)


(ert-deftest test-list-projects-without-session ()
  :tags '(projects)
  (with-test-sandbox
   (let ((response (gitlab-list-projects)))
     (should (vectorp response)))))

(ert-deftest test-list-projects ()
  :tags '(projects)
  (with-test-sandbox
   (with-gitlab-session
    (let ((projects (gitlab-list-projects)))
      (should (<= 0 (length projects)))
      ;;(message "Projects list : %s" (length projects))
      (mapcar (lambda (p)
                (message "Project: %s %s"
                         (assoc-default 'name p)
                         (assoc-default 'id p))
                (should (not (s-blank? (assoc-default 'name p)))))
              projects)))))

(ert-deftest test-list-user-projects ()
  :tags '(projects)
  (with-test-sandbox
   (with-gitlab-session
    (let ((projects (gitlab-list-owned-projects)))
      (should (< 0 (length projects)))
      (mapcar (lambda (p)
                (message "%s" p) ;;(assoc-default 'name p))
                (should (not (s-blank? (assoc-default 'name p)))))
              projects)))))

(ert-deftest test-get-project ()
  :tags '(projects)
  (with-test-sandbox
   (with-gitlab-session
    (let ((project (gitlab-get-project gitlab-project-id)))
      (message "Project : %s %s"
               (assoc-default 'name project)
               (assoc-default 'description project))
      (should (s-equals? gitlab-project-name
                         (assoc-default 'name project)))
      (should (s-equals? gitlab-project-description
                         (assoc-default 'description project)))))))

(ert-deftest test-list-project-events ()
  :tags '(projects)
  (with-test-sandbox
   (with-gitlab-session
    (let ((events (gitlab-list-project-events gitlab-project-id)))
      (should (<= 0 (length events)))
      (mapcar (lambda (e)
                ;; (message "%s" (assoc-default 'author_id e))
                (should (numberp (assoc-default 'author_id e))))
              events)))))

(provide 'gitlab-project-test)
;;; gitlab-projects-test.el ends here
