;; gitlab-mode.el --- Mode for Gitlab

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

;; A major mode for Gitlab

;;; Code:

;;; Code:

(require 'browse-url)
(require 'tabulated-list)

;; Gitlab library

(require 'gitlab-projects)
(require 'gitlab-issues)


;; Core

(defun print-current-line-id ()
  "Display current project."
  (interactive)
  (message (concat "Current ID is: " (tabulated-list-get-id))))



;; Projects

(defun gitlab-goto-project ()
  "Got to web page of the project."
  (let ((project (tabulated-list-get-entry)))
    (browse-url (assoc-default 'web_url project))))

(defun gitlab-show-projects ()
  "Show Gitlab projects."
  (interactive)
  (pop-to-buffer "*Gitlab projects*" nil)
  (gitlab-projects-mode)
  (setq tabulated-list-entries
        (create-projects-entries (gitlab-list-projects)))
  (tabulated-list-print t))

(defun create-projects-entries (projects)
  "Create entries for 'tabulated-list-entries from PROJECTS."
  (mapcar (lambda (p)
            (let ((id (number-to-string (assoc-default 'id p)))
                  (owner (assoc-default 'owner p))
                  (namespace (assoc-default 'namespace p)))
              (list id
                    (vector ;id
                            (assoc-default 'name p)
                            (assoc-default 'name owner)
                            (assoc-default 'name namespace)
                            (assoc-default 'description p)))))
          projects))


;; Issues

(defun gitlab-goto-issue ()
  "Got to web page of the issue."
  )

(defun create-issues-entries (issues)
  "Create entries for 'tabulated-list-entries from ISSUES."
  (mapcar (lambda (i)
            (let ((id (number-to-string (assoc-default 'id i)))
                  (author (assoc-default 'author i)))
              (list id
                    (vector ;id
                     (assoc-default 'state i)
                     (assoc-default 'name author)
                     (assoc-default 'title i)))))
          issues))

(defun gitlab-show-issues ()
  "Show Gitlab issues."
  (interactive)
  (pop-to-buffer "*Gitlab issues*" nil)
  (gitlab-issues-mode)
  (setq tabulated-list-entries
        (create-issues-entries (gitlab-list-issues)))
  (tabulated-list-print t))


;; Gitlab projects mode

(defvar gitlab-projects-mode-hook nil)

(defvar gitlab-projects-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "v") 'print-current-line-id)
    (define-key map (kbd "w") 'gitlab-goto-project)
    map)
  "Keymap for `gitlab-projects-mode' major mode.")

(define-derived-mode gitlab-projects-mode tabulated-list-mode "Gitlab projects"
  "Major mode for browsing Gitlab projects."
  :group 'gitlab
  (setq tabulated-list-format [;("ID" 5 t)
                               ("Name" 25 t)
                               ("Owner"  25 t)
                               ("Namespace" 25 t)
                               ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header))

;; Gitlab issues mode

(defvar gitlab-issues-mode-hook nil)

(defvar gitlab-issues-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "v") 'print-current-line-id)
    (define-key map (kbd "w") 'gitlab-goto-issue)
    map)
  "Keymap for `gitlab-issues-mode' major mode.")

(define-derived-mode gitlab-issues-mode tabulated-list-mode "Gitlab issues"
  "Major mode for browsing Gitlab issues."
  :group 'gitlab
  (setq tabulated-list-format [;("ID" 5 t)
                               ("State" 10 t)
                               ("Author" 20 t)
                               ("Title" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Title" nil))
  (tabulated-list-init-header))






(provide 'gitlab-mode)
;;; gitlab-mode.el ends here
