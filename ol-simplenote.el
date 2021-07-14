;;; ol-simplenote.el --- org links for simplenote -*- coding: utf-8; lexical-binding: t -*-

;; Copyright 2021 FoAM oü
;;
;; Author: nik gaffney <nik@fo.am>
;; Created: 2021-07-14
;; Version: 0.1
;; Package-Requires:  ((emacs "25.1") (simplenote2 "3.0.0") (org "9.4.5"))
;; Keywords: simplenote
;; URL:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; support for links to simplenote notes in org mode

;;; Code:

(require 'ol)
(require 'simplenote2)

;;;###autoload
(with-eval-after-load 'org
  (org-link-set-parameters "simplenote"
                           :follow #'org-simplenote-open
                           :store  #'org-simplenote-store-link))

;;;###autoload
(defun org-simplenote-open (key)
  "Open a note from simplenote using its KEY."
  (simplenote2--open-note
   (simplenote2--filename-for-note key)))

;;;###autoload
(defun org-simplenote-store-link ()
  "Store a link to a note from simplenote."
  (let* ((key (org-simplenote-get-note-key))
         (link (concat "simplenote:" key))
           (description (org-simplenote-get-note-title)))
      (org-link-store-props
       :type "simplenote"
       :link link
       :description description)))

(defun org-simplenote-get-note-key ()
  "Extract the key of the note displayed in the current buffer."
  ;; the note 'key' is equivalent to the file name of the note
  (seq-drop
   (buffer-file-name)
   (+ 6 (length simplenote2-directory))))

(defun org-simplenote-get-note-title ()
  "Extract the title of a note in the current buffer."
  (format "%s" (buffer-name)))


(provide 'ol-simplenote)

;;; ol-simplenote.el ends here
