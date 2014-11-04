;;; gitlab-api.el --- Gitlab API settings.

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

;;; Code:

(defvar gitlab-host nil
  "The Gitlab API endpoint.")

(defvar gitlab-username nil
  "The Gitlab username.")

(defvar gitlab-password nil
  "The Gitlab account's password.")

(defvar gitlab-token-id nil
  "The Gitlab tokenID to perform HTTP requests.")

(defconst gitlab-api-version "v3"
  "The Gitlab API version.")


(provide 'gitlab-api)
;;; gitlab-api.el ends here
