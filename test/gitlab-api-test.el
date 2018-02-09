;;; gitlab-api-test.el --- Tests for API commons

;; Copyright (C) 2015 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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

(defun check-gitlab-settings-from-environment (var value fct)
  (let ((backup (getenv var)))
    (unwind-protect
        (progn
          (setenv var value)
          (should (string= value (funcall fct))))
      (setenv var backup))))

(ert-deftest test-gitlab-get-host-from-environment ()
  :tags '(api)
  (with-test-sandbox
   (check-gitlab-settings-from-environment
    "GITLAB_HOST" "http://localhost:8989" 'gitlab--get-host)))

(ert-deftest test-gitlab-get-host-from-conf ()
  :tags '(api)
  (with-test-sandbox
   (let* ((value "http://localhost:7878")
          (gitlab-host value))
     (should (string= value (gitlab--get-host))))))

(provide 'gitlab-api-test)
;;; gitlab-api-test.el ends here
