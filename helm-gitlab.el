;;; helm-gitlab.el --- Helm interface to Gitlab

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; URL: https://github.com/nlamirault/emacs-gitlab
;; Version: 0.1.0
;; Keywords: gitlab, helm

;; Package-Requires: ((cl-lib "0.5") (helm "1.0"))

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

;; Provides a Helm interface to Gitlab


;;; Code:

;;(require 'cl-lib)
(require 'browse-url)
(require 'helm)
(require 's)

;; Customization

(defgroup helm-gitlab nil
  "Helm interface for Emacs."
  :group 'gitlab
  :link '(url-link :tag "Github" "https://github.com/nlamirault/emacs-gitlab")
  :link '(emacs-commentary-link :tag "Commentary" "emacs-gitlab"))

;; Gitlab library

(require 'gitlab)

;; UI

(defface helm-gitlab-title
  '((((class color) (background light)) :foreground "red" :weight semi-bold)
    (((class color) (background dark)) :foreground "green" :weight semi-bold))
  "face of post title"
  :group 'helm-gitlab)


;; Core

(defun helm-gitlab-projects-init ()
  (when (s-blank? gitlab-token-id)
    (gitlab-login gitlab-username gitlab-password))
  (let ((projects (gitlab-list-projects))
        result)
    (mapcar (lambda (p)
              (list (cons 'page (assoc-default 'web_url p))
                    (cons 'description (assoc-default 'description p))
                    (cons 'ssh (assoc-default 'ssh_url_to_repo p))))
            projects)))

    ;; (sort (cl-loop with posts = (assoc-default 'items json-res)
    ;;                for post across posts
    ;;                for points = (assoc-default 'points post)
    ;;                for title = (assoc-default 'title post)
    ;;                for url = (assoc-default 'url post)
    ;;                for comments = (assoc-default 'commentCount post)
    ;;                for id = (assoc-default 'id post)
    ;;                for cand = (format "%s %s (%d comments)"
    ;;                                   (format "[%d]" points)
    ;;                                   (propertize title 'face 'helm-hackernews-title)
    ;;                                   comments)
    ;;                collect
    ;;                (cons cand
    ;;                      (list :url url :points points
    ;;                            :post-url (format "https://news.ycombinator.com/item?id=%s" id))))
    ;;       'helm-hackernews-sort-predicate)))


(defvar helm-gitlab-projects-source
  '((name . "Gitlab projects")
    (candidates . helm-gitlab-projects-init)
    (action . (("Browse Link" . helm-gitlab-project-browse-link)
               ("Browse Project Page"  . helm-gitlab-project-browse-page)))
    (candidate-number-limit . 9999)))


;; API

;;;###autoload
(defun helm-projects ()
  (interactive)
  (helm :sources '(helm-gitlab-projects-source) :buffer "*helm-gitlab*"))

(provide 'helm-gitlab)
;;; helm-gitlab.el ends here
