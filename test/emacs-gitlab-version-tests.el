;;; emacs-gitlab-version-tests.el --- Tests for version information

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

(ert-deftest emacs-gitlab-mode-library-version ()
  :expected-result (if (executable-find "cask") :passed :failed)
  ;;  The default directory must end with a slash
  (let* ((cask-version (car (process-lines "cask" "version")))
	 ;;(lib-version (emacs-gitlab-mode-library-version)))
	 )
    ;;(message "Emacs-Gitlab.el : %s" lib-version)
    (message "emacs-gitlab Cask version: %s" cask-version)
    ;;(should (string= version (emacs-gitlab-mode-library-version)))))
    (should (string= "0.1.0" cask-version))))


(provide 'emacs-gitlab-version-test)
;;; emacs-gitlab-version-tests.el ends here
