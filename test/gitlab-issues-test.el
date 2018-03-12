;;; gitlab-issues-test.el --- Tests for Gitlab issues API

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

;;; Code:

(require 's)

(ert-deftest test-list-issues-without-session ()
  :tags '(issues)
  (with-test-sandbox
   (let ((response (gitlab-list-issues 1 20)))
     (should (s-equals? response
                        "Error (gitlab): HTTP GET Error 401 on URI: issues" )))))

(ert-deftest test-list-issues ()
  :tags '(issues)
  (with-test-sandbox
   (with-gitlab-session
    (let ((issues (gitlab-list-issues 1 20)))
      (should (<= 0 (length issues)))
      (mapcar (lambda (i)
                (should (not (s-blank? (assoc-default 'title i))))
                (should (numberp (assoc-default 'project_id i)))
                (message "Issue: %s" (assoc-default 'title i)))
              issues)))))

(ert-deftest test-list-project-issues ()
  :tags '(issues current)
  (with-test-sandbox
   (with-gitlab-session
    (let ((issues (gitlab-list-project-issues gitlab-project-id)))
      (should (<= 0 (length issues)))
      (mapcar (lambda (i)
                (should (not (s-blank? (assoc-default 'title i))))
                (should (not (s-blank? (assoc-default 'state i))))
                (should (= (string-to-number gitlab-project-id)
                           (assoc-default 'project_id i)))
                (message "%s" i)
                (message "%s" (assoc-default 'title i)))
              issues)))))

(ert-deftest test-get-single-issue ()
  :tags '(issues current)
  (with-test-sandbox
   (with-gitlab-session
    (let ((issue (gitlab-get-issue gitlab-project-id gitlab-issue-id)))
      (message "Issue : %s" issue)
      (should (s-equals? gitlab-issue-title
                         (assoc-default 'title issue)))))))

(provide 'gitlab-issues-test)
;;; gitlab-issues-test.el ends here
