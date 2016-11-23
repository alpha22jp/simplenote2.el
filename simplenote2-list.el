;;; simplenote2-list.el ---                       -*- lexical-binding: t; -*-

;; Copyright (C) 2016  alpha22jp

;; Author: alpha22jp <alpha22jp@gmail.com>
;; Keywords: 

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

;; 

;;; Code:
(require 'cl-lib)

(defconst simplenote2-list-buffer-name "*Simplenote List*")

(defun simplenote2-list-refresh-mark ()
  "Refresh all mark in simplenote list buffer."
  (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (file-exists-p (simplenote2--filename-for-note-marked-deleted
                              (tabulated-list-get-id)))
          (tabulated-list-put-tag "D"))
        (forward-line))))

(defun simplenote2-list-get-entry (key note-info)
  "Get list entry for note specified by KEY and NOTE-INFO."
  (let ((note (or (simplenote2--get-file-string
                   (simplenote2--filename-for-note key))
                  (simplenote2--get-file-string
                   (simplenote2--filename-for-note-marked-deleted key))))
        (time (seconds-to-time (nth 3 note-info))))
    (list key `[,(concat (and (nth 6 note-info) "* ")
                         (simplenote2--note-headline note))
                ,(format-time-string "%Y/%m/%d %H:%M" time)
                ,(simplenote2--note-headrest note)])))

(defun simplenote2-list-refresh-entries ()
  "Refresh simplenote list entries."
  (setq tabulated-list-entries
        (cl-loop for key being the hash-keys of simplenote2-notes-info
           using (hash-values note-info)
           collect (simplenote2-list-get-entry key note-info))))

(defun simplenote2-list-order-predicate (a b)
  "Predicate function to determine the order between A and B."
  (cond ((nth 6 (gethash (car a) simplenote2-notes-info)) t)
        ((nth 6 (gethash (car b)  simplenote2-notes-info)) nil)
        (t (> (nth 3 (gethash (car a) simplenote2-notes-info))
              (nth 3 (gethash (car b) simplenote2-notes-info))))))

(defun simplenote2-list-open-note ()
  "Open note at which the current line points."
  (interactive)
  (simplenote2--open-note
   (simplenote2--filename-for-note (tabulated-list-get-id))))

(defun simplenote2-list-mark-for-deletion ()
  "Mark note for deletion at which the current line points."
  (interactive)
  (simplenote2--mark-note-for-deletion (tabulated-list-get-id))
  (tabulated-list-put-tag "D" t))

(defun simplenote2-list-unmark-for-deletion ()
  "Unmark note for deletion at which the current line points."
  (interactive)
  (simplenote2--unmark-note-for-deletion (tabulated-list-get-id))
  (tabulated-list-put-tag " " t))

(defvar simplenote2-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\C-m" 'simplenote2-list-open-note)
    (define-key map "a" 'simplenote2--create-note-locally)
    (define-key map "g" 'simplenote2-sync-notes)
    (define-key map "d" 'simplenote2-list-mark-for-deletion)
    (define-key map "u" 'simplenote2-list-unmark-for-deletion)
    map)
  "Local keymap for `simplenote2-list-mode' buffers.")

(define-derived-mode simplenote2-list-mode tabulated-list-mode "Simplenote List"
  "Major mode for Simplenote List"
  (setq tabulated-list-format
        `[("Header" 20 nil)
          ("Modified date" 16 simplenote2-list-order-predicate)
          ("Description" 40 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Modified date" nil))
  (tabulated-list-init-header))

(defun simplenote2-list-refresh ()
  "Refresh simplenote list."
  (simplenote2-list-refresh-entries)
  (tabulated-list-print)
  (simplenote2-list-refresh-mark))

;;;###autoload
(defun simplenote2-list ()
  "Show Simplenote List buffer."
  (interactive)
  (let ((buffer (get-buffer simplenote2-list-buffer-name)))
    (unless buffer
      (with-current-buffer (get-buffer-create simplenote2-list-buffer-name)
        (setq buffer (current-buffer))
        (simplenote2-list-mode)
        (simplenote2-list-refresh)))
    (switch-to-buffer buffer)))

(provide 'simplenote2-list)
;;; simplenote2-list.el ends here
