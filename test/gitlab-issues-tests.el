;;; gitlab-issues-tests.el --- Tests for Gitlab issues API

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

(require 'gitlab)
(require 'gitlab-test-utils)

(ert-deftest test-list-issues-without-session ()
  (should-error (gitlab-list-issues)))

(ert-deftest test-list-issues ()
  (with-gitlab-session
   (let ((issues (gitlab-list-issues)))
     (should (< 0 (length issues)))
     (mapcar (lambda (i)
               (should (not (s-blank? (assoc-default 'title i))))
               (should (numberp (assoc-default 'project_id i)))
               (message "%s" (assoc-default 'title i)))
             issues))))

(provide 'gitlab-issues-tests)
;;; gitlab-issues-tests.el ends here
