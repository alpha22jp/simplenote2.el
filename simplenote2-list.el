;;; simplenote2-list.el --- Simplenote browser based on tabulated list

;; Copyright (C) 2016  alpha22jp

;; Author: alpha22jp <alpha22jp@gmail.com>
;; Keywords: simplenote tabulated

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

;; This is a new browser UI for simplenote2 based on tabulated-list-mode.

;;; Code:

(defcustom simplenote2-list-format
  '((header "Header" 24 nil identity)
    (modified "Modified" 16 simplenote2-list-order-predicate
              (lambda (date) (format-time-string "%Y/%m/%d %H:%M" date)))
    (tags "Tags" 10 nil
          (lambda (tags)
            (mapconcat (lambda (tag) (format "[%s]" tag)) tags " ")))
    (description "Description" 0 nil identity))
  "Format for Simplenote list."
  :type '(alist :key-type
          (choice
           (const :tag "Id" id)
           (const :tag "Header" header)
           (const :tag "Modified date" modified)
           (const :tag "Description" description)
           (const :tag "Tags" tags))
          :value-type
          (list
           (string :tag "Label")
           (integer :tag "Field length")
           (boolean :tag "Sortable")
           (choice
            (string :tag "Format")
            (function :tag "Formatter"))))
  :group 'simplenote2)

(defcustom simplenote2-list-sort-key '("Modified" . nil)
  "Sort key for Simplenote list."
  :type '(cons string boolean)
  :group 'simplenote2)

(defconst simplenote2--list-buffer-name "*Simplenote List*")

(defun simplenote2--list-refresh-mark ()
  "Refresh all mark in simplenote list buffer."
  (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (simplenote2--is-note-trashed (tabulated-list-get-id))
          (tabulated-list-put-tag "D"))
        (forward-line))))

(defun simplenote2-list-get-entry (file)
  "Get list entry for note specified by FILE."
  (let* ((key (file-name-nondirectory file))
         (note-info (simplenote2--get-note-info key))
         (date (simplenote2--file-mtime file))
         (note (simplenote2--get-file-string file))
         (header (concat (and (nth 6 note-info) "* ")
                         (simplenote2--note-headline note)))
         (desc (concat (simplenote2--note-headrest note)))
         (tags (nth 4 note-info)))
    (list key
          (apply 'vector
                 (cl-loop for (sym label width sort format) in simplenote2-list-format
                          collect (let ((value (cond ((eq sym 'id) key)
                                                     ((eq sym 'modified) date)
                                                     ((eq sym 'header) header)
                                                     ((eq sym 'description) desc)
                                                     ((eq sym 'tags) tags))))
                                    (funcall (if (stringp format)
                                                 (lambda (val) (funcall 'format format val))
                                               format)
                                             value)))))))

(defun simplenote2--list-refresh-entries ()
  "Refresh simplenote list entries."
  (setq tabulated-list-entries
        (append
         (cl-loop for file in simplenote2--filtered-new-notes-list
                  collect (simplenote2-list-get-entry file))
         (cl-loop for file in simplenote2--filtered-notes-list
                  when (simplenote2--note-filtered-by-tag-p file)
                  collect (simplenote2-list-get-entry file))
         (cl-loop for file in simplenote2--filtered-trash-notes-list
                  when (simplenote2--note-filtered-by-tag-p file)
                  collect (simplenote2-list-get-entry file)))))

(defun simplenote2-list-order-predicate (a b)
  "Predicate function to determine the order between A and B."
  (let* ((key-a (car a)) (key-b (car b))
         (note-info-a (simplenote2--get-note-info key-a))
         (note-info-b (simplenote2--get-note-info key-b)))
    (cond ((simplenote2--is-note-new key-a) t) ;; new note should be on the top
          ((simplenote2--is-note-new key-b) nil)
          ((nth 6 note-info-a) t) ;; pinned note should be on the top
          ((nth 6 note-info-b) nil)
          (t (> (nth 3 note-info-a) (nth 3 note-info-b))))))

(defun simplenote2-list-open-note ()
  "Open note at which the current line points."
  (interactive)
  (let ((key (tabulated-list-get-id)))
    (simplenote2--open-note
     (if (simplenote2--is-note-new key)
         (simplenote2--filename-for-newnote key)
       (simplenote2--filename-for-note key)))))

(defun simplenote2-list-mark-for-deletion ()
  "Mark note for deletion at which the current line points."
  (interactive)
  (let* ((key (tabulated-list-get-id))
         (note-info (simplenote2--get-note-info key)))
    (if (simplenote2--is-note-new key)
        (when (yes-or-no-p
               "This note hasn't been synced to server. Do you delete it immediately?")
          (simplenote2--delete-note-locally (simplenote2--filename-for-newnote key))
          (simplenote2-list-refresh))
      (simplenote2--mark-note-for-deletion (tabulated-list-get-id))
      (tabulated-list-put-tag "D" t))))

(defun simplenote2-list-unmark-for-deletion ()
  "Unmark note for deletion at which the current line points."
  (interactive)
  (simplenote2--unmark-note-for-deletion (tabulated-list-get-id))
  (tabulated-list-put-tag " " t))

(defun simplenote2-list-filter-notes ()
  "Filter notes on Simplenote list screen by regexp input."
  (interactive)
  (let ((regexp (read-string "Input regexp: ")))
    (setq simplenote2--filter-regexp (if (> (length regexp) 0) regexp nil))
    (simplenote2-browser-refresh)))

(defun simplenote2-list-toggle-filter-condition ()
  "Toggle filter condition between AND and OR."
  (interactive)
  (simplenote2--toggle-filter-condition)
  (simplenote2-browser-refresh))

(defvar simplenote2-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\C-m" 'simplenote2-list-open-note)
    (define-key map "a" 'simplenote2--create-note-locally)
    (define-key map "g" 'simplenote2-sync-notes)
    (define-key map "d" 'simplenote2-list-mark-for-deletion)
    (define-key map "t" 'simplenote2-filter-note-by-tag)
    (define-key map "u" 'simplenote2-list-unmark-for-deletion)
    (define-key map "/" 'simplenote2-list-filter-notes)
    (define-key map "^" 'simplenote2-list-toggle-filter-condition)
    map)
  "Local keymap for `simplenote2-list-mode' buffers.")

(define-derived-mode simplenote2-list-mode tabulated-list-mode "Simplenote List"
  "Major mode for Simplenote List"
  (setq tabulated-list-format
        (apply 'vector
               (cl-loop for (sym label width sort format) in simplenote2-list-format
                        collect (list label width sort))))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key simplenote2-list-sort-key)
  (tabulated-list-init-header))

(defun simplenote2-list-refresh ()
  "Refresh simplenote list."
  (simplenote2--filter-note-list)
  (simplenote2--list-refresh-entries)
  (tabulated-list-print t)
  (simplenote2--list-refresh-mark))

;;;###autoload
(defun simplenote2-list ()
  "Show Simplenote List buffer."
  (interactive)
  (let ((buffer (get-buffer simplenote2--list-buffer-name)))
    (unless buffer
      (with-current-buffer (get-buffer-create simplenote2--list-buffer-name)
        (setq buffer (current-buffer))
        (simplenote2-list-mode)
        (simplenote2-list-refresh)))
    (switch-to-buffer buffer)))

(provide 'simplenote2-list)
;;; simplenote2-list.el ends here
