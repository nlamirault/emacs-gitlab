;;; helm-gitlab.el --- Helm interface to Gitlab

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; URL: https://github.com/nlamirault/emacs-gitlab
;; Version: 0.1.0
;; Keywords: gitlab, helm

;; Package-Requires: ((s "1.9.0") (dash "2.9.0") (helm "1.0") (gitlab "0.8.0"))

;; Copyright (C) 2014, 2015, 2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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

;; Provides a Helm interface to Gitlab


;;; Code:

(require 'browse-url)
(require 'dash)
(require 'helm)
(require 'helm-buffers)
(require 's)

;; Customization

(defgroup helm-gitlab nil
  "Helm interface for Emacs."
  :group 'gitlab
  :link '(url-link :tag "Github" "https://github.com/nlamirault/emacs-gitlab")
  :link '(emacs-commentary-link :tag "Commentary" "emacs-gitlab"))

;; Gitlab library

(require 'gitlab-api)
(require 'gitlab-session)
(require 'gitlab-projects)
(require 'gitlab-issues)
(require 'gitlab-ui)


;; UI
;; ----

(defface helm-gitlab--title
  '((((class color) (background light)) :foreground "red" :weight semi-bold)
    (((class color) (background dark)) :foreground "green" :weight semi-bold))
  "Face of Gitlab information"
  :group 'helm-gitlab)


;; Core
;; ------

(defun helm-gitlab--project-issues-init (project-id)
  (with-gitlab-auth
   (let ((issues (gitlab-list-project-issues project-id)))
     (mapcar (lambda (i)
               (cons (format "[%s] %s [%s]"
                             (assoc-default 'id i)
                             (propertize (assoc-default 'title i)
                                         'face
                                         'helm-gitlab--title)
                             (assoc-default 'state i))
                     (list :project-id (assoc-default 'project_id i)
                           :issue-id (assoc-default 'id i)
                           :name (assoc-default 'title i))))
             issues))))


(defvar helm-gitlab--project-issues-source
  '((name . "Gitlab project issues")
    (candidates . helm-gitlab--project-issues-init)
    (action . (("Browse Link" . helm-gitlab--issue-browse-link)
               ("Show issue" . helm-gitlab--issue-show)))))


(defun helm-gitlab--projects-init ()
  (with-gitlab-auth
   (let ((projects (gitlab-list-all-projects)))
     (mapcar (lambda (p)
               (cons (format "%s" (propertize (assoc-default 'name p)
                                              'face
                                              'helm-gitlab--title))
                     (list :page (assoc-default 'web_url p)
                           :name (assoc-default 'name p)
                           :project-id (assoc-default 'id p))))
             projects))))


(defun helm-gitlab--project-browse-page (cast)
  (browse-url (plist-get cast :page)))


(defvar helm-gitlab--projects-source
  '((name . "Gitlab projects")
    (candidates . helm-gitlab--projects-init)
    (action . (("Browse project page" . helm-gitlab--project-browse-page)
               ("Issues" . helm-gitlab--project-issues)
               ("Members" . helm-gitlab--project-members)
               ))
    (candidate-number-limit . 9999)))


(defun helm-gitlab--issue-show (issue)
  (let ((buf (get-buffer-create helm-gitlab--buffer-name)))
    (switch-to-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (save-excursion
        (gitlab-mode)
        issue))))



(defun helm-gitlab--issues-init ()
  (with-gitlab-auth
   (let ((issues (gitlab-list-all-issues)))
     (mapcar (lambda (i)
               (cons (format "[%s] %s [%s]"
                             (assoc-default 'id i)
                             (propertize (assoc-default 'title i)
                                         'face
                                         'helm-gitlab--title)
                             (assoc-default 'state i))
                     (list :project-id (assoc-default 'project_id i)
                           :issue-id (assoc-default 'id i)
                           :name (assoc-default 'title i))))
             issues))))


(defun helm-gitlab--issue-browse-link (cand)
  (browse-url
   (gitlab-projects--get-issue-link (plist-get cand :project-id)
                                 (plist-get cand :issue-id))))


(defvar helm-gitlab--issues-source
  '((name . "Gitlab issues")
    (candidates . helm-gitlab--issues-init)
    (action . (("Browse Link" . helm-gitlab--issue-browse-link)
               ("Show issue" . helm-gitlab--issue-show)))))




;; API

;;;###autoload
(defun helm-gitlab-projects ()
  "List Gitlab projects using Helm interface."
  (interactive)
  (helm :sources '(helm-gitlab--projects-source)
        :buffer helm-gitlab--buffer-name))


;;;###autoload
(defun helm-gitlab-project-issues ()
  "List Gitlab projects using Helm interface."
  (interactive "P")
  (helm :sources helm-gitlab--project-issues-source
        :prompt "Project : "
        :buffer helm-gitlab--buffer-name))


;;;###autoload
(defun helm-gitlab-issues ()
  "List Gitlab issues using Helm interface."
  (interactive)
  (helm :sources '(helm-gitlab--issues-source)
        :buffer helm-gitlab--buffer-name))

(provide 'helm-gitlab)
;;; helm-gitlab.el ends here
