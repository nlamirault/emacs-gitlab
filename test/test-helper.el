;; test-helper.el --- Test helpers for gitlab.el

;; Copyright (C) Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; Author: Nicolas Lamirault <nicolas.lamirault@chmouel.com>
;; Homepage: https://github.com/nlamirault/emacs-gitlab

;;; License:

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ansi)
(require 'f)
(require 'undercover)

(setq debugger-batch-max-lines (+ 50 max-lisp-eval-depth)
      debug-on-error t)

(defvar username (getenv "HOME"))

(defconst gitlab-testsuite-dir
  (f-parent (f-this-file))
  "The testsuite directory.")

(defconst gitlab-source-dir
  (f-parent gitlab-testsuite-dir)
  "The gitlab.el source directory.")

(defconst gitlab-sandbox-path
  (f-expand "sandbox" gitlab-testsuite-dir)
  "The sandbox path for gitlab.")

(defun cleanup-load-path ()
  "Remove home directory from 'load-path."
  (message (ansi-green "[gitlab] Cleanup path"))
  (mapc #'(lambda (path)
            (when (string-match (s-concat username "/.emacs.d") path)
              (message (ansi-yellow "Suppression path %s" path))
              (setq load-path (delete path load-path))))
        load-path))

(defun load-unit-tests (path)
  "Load all unit test from PATH."
  (message (ansi-green "[gitlab] Execute unit tests %s"
                       path))
  (dolist (test-file (or argv (directory-files path t "-test.el$")))
    (load test-file nil t)))


(defun load-library (file)
  "Load current library from FILE."
  (let ((path (s-concat gitlab-source-dir file)))
    (message (ansi-yellow "[gitlab] Load library from %s" path))
    (undercover "*.el" (:exclude "*-test.el"))
    (require 'gitlab))); path)))


(defun setup-gitlab ()
  "Setup Gitlab for unit test."
  )


(provide 'test-helper)
;;; test-helper.el ends here
