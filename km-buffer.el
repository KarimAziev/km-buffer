;;; km-buffer.el --- Configure buffer -*- lexical-binding: t -*-

;; Copyright © 2020-2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-buffer
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "28.1") (project "0.9.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file configures operations with buffer

;;; Code:




(declare-function dired-get-marked-files "dired")

(defcustom km-buffer-backup-directory "~/.backups"
  "Directory to save backup files by `km-buffer-make-backup'."
  :type 'directory
  :group 'km)

(defcustom km-buffer-backup-time-format "%Y_%m_%d_%H_%M_%S"
  "Format given to `format-time-string' which is appended to the filename."
  :type 'directory
  :group 'km)


;;;###autoload
(defun km-buffer-delete-current-buffer-file ()
  "Remove file and kill current buffer without prompt."
  (interactive)
  (when-let* ((buffer (current-buffer))
              (filename (buffer-file-name buffer)))
    (when (and buffer (buffer-live-p buffer))
      (save-buffer)
      (kill-buffer buffer))
    (delete-file filename t)))


;;;###autoload
(defun km-buffer-reload-current-buffer ()
  "Kill current buffer and reopen its `buffer-file-name."
  (interactive)
  (when-let ((file buffer-file-name)
             (buff (current-buffer))
             (pos (point)))
    (when (or (not (buffer-modified-p))
              (and (yes-or-no-p "Save buffer?")
                   (progn (save-buffer) t)))
      (kill-buffer buff)
      (find-file file)
      (goto-char pos))))


;;;###autoload
(defun km-buffer-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun km-buffer-get-parent-target-dir (&optional dont-create)
  "Return directory in TARGET-DIR with name of SOURCE-DIR.
If DONT-CREATE is non nil, don't create it if it doesn't exists."
  (let* ((containing-dir default-directory)
         (backup-container
          (format "%s/%s"
                  km-buffer-backup-directory
                  containing-dir)))
    (unless (or dont-create
                (file-exists-p backup-container))
      (make-directory backup-container t))
    backup-container))


;;;###autoload
(defun km-buffer-find-backup-file ()
  "Find a file in `km-buffer-backup-directory'."
  (interactive)
  (if (not (file-exists-p km-buffer-backup-directory))
      (message "Directory %s doesn't exist"
               km-buffer-backup-directory)
    (completing-read "File: "
                     (directory-files-recursively
                      km-buffer-backup-directory
                      directory-files-no-dot-files-regexp
                      nil))))


;;;###autoload
(defun km-buffer-find-backup-dir ()
  "Find a backup mirror of current file."
  (interactive)
  (if (file-exists-p km-buffer-backup-directory)
      (find-file km-buffer-backup-directory)
    (message "Directory %s doesn't exist"
             km-buffer-backup-directory)))


;;;###autoload
(defun km-buffer-find-backup-mirror ()
  "Find a backup mirror of current file."
  (interactive)
  (let* ((containing-dir default-directory)
         (backup-container
          (format "%s/%s"
                  km-buffer-backup-directory
                  containing-dir)))
    (if (not (file-exists-p backup-container))
        (message "Directory %s doesn't exist"
                 km-buffer-backup-directory)
      (read-file-name "File: " (and buffer-file-name
                                    (file-name-nondirectory
                                     buffer-file-name))
                      backup-container))))


;;;###autoload
(defun km-buffer-make-backup ()
  "Make a backup copy of current file or `dired' marked files.
If in `dired', backup current file or marked files.
See also `km-buffer-backup-time-format'."
  (interactive)
  (let ((new-file-name-base))
    (when buffer-file-name
      (setq new-file-name-base (concat (file-name-nondirectory
                                        buffer-file-name)
                                       "~"
                                       (format-time-string
                                        km-buffer-backup-time-format))))
    (when (and km-buffer-backup-directory
               (not (file-exists-p km-buffer-backup-directory)))
      (make-directory km-buffer-backup-directory t))
    (if-let ((dir (and new-file-name-base
                       (km-buffer-get-parent-target-dir))))
        (let ((backup-name
               (expand-file-name new-file-name-base dir)))
          (unless (file-exists-p dir)
            (make-directory dir t))
          (copy-file buffer-file-name backup-name t)
          (message " %s"(concat "Backup saved at: "
                                backup-name)))
      (when (eq major-mode 'dired-mode)
        (mapc (lambda (x)
                (let ((backup-name
                       (concat x "~" (format-time-string
                                      km-buffer-backup-time-format)
                               "~")))
                  (copy-file x backup-name t)))
              (dired-get-marked-files))))))

(defun km-buffer--kill-fn-result (fn &rest args)
  "Kill result of applying FN with ARGS or show ERR-MESSAGE."
  (if buffer-file-name
      (if-let ((result (apply fn args)))
          (progn (kill-new result)
                 (message "Copied %s" result))
        (message "No result"))
    (message "No filename.")))

(defun km-buffer-current-file-relative-project-name ()
  "Convert buffer filename to be relative to project root directory."
  (require 'project)
  (require 'vc nil t)
  (when-let ((curr (ignore-errors (when (fboundp 'project-root)
                                    (project-root
                                     (project-current))))))
    (file-relative-name
     (or buffer-file-name default-directory)
     curr)))


;;;###autoload
(defun km-buffer-kill-current-file-relative-project-name ()
  "Copy relative filename of current buffer to project root directory."
  (interactive)
  (km-buffer--kill-fn-result 'km-buffer-current-file-relative-project-name))

(defun km-buffer-current-file--basename ()
  "Return the base name of the current filename: no directory, no extension."
  (when buffer-file-name
    (file-name-base buffer-file-name)))


;;;###autoload
(defun km-buffer-kill-current-filename-base ()
  "Copy the base name of the current filename: no directory, no extension."
  (interactive)
  (km-buffer--kill-fn-result 'km-buffer-current-file--basename))

(defun km-buffer-file-name--nondirectory ()
  "Return the filename of current buffer file sans its directory."
  (when buffer-file-name
    (file-name-nondirectory buffer-file-name)))


;;;###autoload
(defun km-buffer-kill-file-name-nondirectory ()
  "Copy the filename of current buffer file sans its directory."
  (interactive)
  (km-buffer--kill-fn-result 'km-buffer-file-name--nondirectory))


;;;###autoload
(defun km-buffer-kill-absolute-filename ()
  "Copy the absolute path to current file."
  (interactive)
  (when buffer-file-name
    (kill-new buffer-file-name)
    (message "Copied %s"
             buffer-file-name)))


;;;###autoload
(defun km-buffer-kill-absolute-filename-abbreviated ()
  "Copy a version of current filename shortened using `directory-abbrev-alist'."
  (interactive)
  (when buffer-file-name
    (let ((res (abbreviate-file-name buffer-file-name)))
      (kill-new (abbreviate-file-name buffer-file-name))
      (message "Copied %s"
               res))))


;;;###autoload
(defun km-buffer-kill-directory ()
  "Copy a current directory."
  (interactive)
  (kill-new (if buffer-file-name
                (file-name-directory buffer-file-name)
              default-directory))
  (message "Copied %s" (if buffer-file-name
                           (file-name-directory buffer-file-name)
                         default-directory)))


;;;###autoload
(defun km-buffer-kill-directory-abbreviated ()
  "Copy abreviated filename of current directory."
  (interactive)
  (let ((res (abbreviate-file-name (if buffer-file-name
                                       (file-name-directory buffer-file-name)
                                     default-directory))))
    (kill-new res)
    (message "Copied %s" res)))


;;;###autoload
(defun km-buffer-sqlite-open-file ()
  "Open current file with `sqlite-mode-open-file'."
  (interactive)
  (sqlite-mode-open-file
   (read-file-name "SQLite file name: "
                   (when buffer-file-name
                     (file-name-nondirectory
                      buffer-file-name)))))

(defun km-buffer-view-echo-messages-0 ()
  "Switch to the *Messages* buffer with recent echo-area messages."
  (let ((curr (current-buffer))
        (msg-buff (messages-buffer))
        (msg-window))
    (cond ((eq curr msg-buff)
           (goto-char (point-max))
           (when (re-search-backward "[^\n\s\t]" nil t 1)
             (forward-char 1))
           (recenter-top-bottom))
          ((setq msg-window (get-buffer-window msg-buff))
           (select-window msg-window)
           (goto-char (point-max))
           (when (re-search-backward "[^\n\s\t]" nil t 1)
             (forward-char 1))
           (recenter-top-bottom))
          (t (switch-to-buffer-other-window msg-buff t)
             (with-current-buffer msg-buff
               (goto-char (point-max))
               (when (re-search-backward "[^\n\s\t]" nil t 1)
                 (forward-char 1))
               (recenter-top-bottom))))
    (with-current-buffer msg-buff
      (visual-line-mode 1))))

;;;###autoload
(defun km-buffer-view-echo-messages ()
  "Switch to the *Messages* buffer with recent echo-area messages."
  (interactive)
  (if (minibuffer-window-active-p (selected-window))
      (with-minibuffer-selected-window
        (km-buffer-view-echo-messages-0))
    (km-buffer-view-echo-messages-0)))

;;;###autoload (autoload 'km-buffer-kill-path-menu "km-buffer.el" nil t)
(transient-define-prefix km-buffer-kill-path-menu ()
  "Command dispatcher for copying current buffer file name in misc formats."
  ["Copy filename"
   ("f"  km-buffer-kill-absolute-filename
    :description (lambda ()
                   (concat (or buffer-file-name
                               "")))
    :inapt-if-not buffer-file-name)
   ("a"  km-buffer-kill-absolute-filename-abbreviated
    :description (lambda ()
                   (concat (or (and buffer-file-name
                                    (abbreviate-file-name buffer-file-name))
                               "")))
    :inapt-if-not (lambda ()
                    (and buffer-file-name
                         (abbreviate-file-name buffer-file-name))))]
  ["Copy directory"
   ("d"  km-buffer-kill-directory
    :description (lambda ()
                   (concat (or (if buffer-file-name
                                   (file-name-directory buffer-file-name)
                                 default-directory)
                               "")))
    :inapt-if-not (lambda ()
                    (if buffer-file-name
                        (file-name-directory buffer-file-name)
                      default-directory)))
   ("D"  km-buffer-kill-directory-abbreviated
    :description (lambda ()
                   (abbreviate-file-name
                    (concat (or (if buffer-file-name
                                    (file-name-directory
                                     buffer-file-name)
                                  default-directory)
                                ""))))
    :inapt-if-not (lambda ()
                    (if buffer-file-name
                        (file-name-directory buffer-file-name)
                      default-directory)))]
  ["Copy relative filename"
   ("p"
    km-buffer-kill-current-file-relative-project-name
    :description (lambda ()
                   (or (km-buffer-current-file-relative-project-name)
                       "No project"))
    :inapt-if-not km-buffer-current-file-relative-project-name
    :if (lambda ()
          (not (equal (km-buffer-file-name--nondirectory)
                      (km-buffer-current-file-relative-project-name)))))
   ("n"  km-buffer-kill-file-name-nondirectory
    :description (lambda ()
                   (concat (or (km-buffer-file-name--nondirectory)
                               "")))
    :inapt-if-not km-buffer-file-name--nondirectory)
   ("b"  km-buffer-kill-current-filename-base
    :description (lambda ()
                   (concat (or (km-buffer-current-file--basename)
                               "")))
    :inapt-if-not buffer-file-name)])


;;;###autoload (autoload 'km-buffer-actions-menu "km-buffer.el" nil t)
(transient-define-prefix km-buffer-actions-menu ()
  "Command dispatcher with buffer commands."
  ["Buffer actions"
   ("G" "Revert buffer" revert-buffer :inapt-if-not
    buffer-modified-p)
   ("k" "Kill current buffer" kill-current-buffer)]
  [:description
   "File actions\n"
   ("r" km-buffer-reload-buffer
    :description (lambda ()
                   (concat "Reload "
                           (propertize (copy-sequence (or
                                                       (and buffer-file-name
                                                            (abbreviate-file-name
                                                             buffer-file-name))
                                                       ""))
                                       'face 'transient-argument)))
    :inapt-if-not buffer-file-name)
   ("D"
    km-buffer-delete-current-buffer-file
    :description (lambda ()
                   (concat "Delete "
                           (propertize (copy-sequence (or
                                                       (and buffer-file-name
                                                            (abbreviate-file-name
                                                             buffer-file-name))
                                                       ""))
                                       'face 'transient-argument)))
    :inapt-if-not buffer-file-name)
   ("R" km-buffer-rename-current-buffer-file
    :description (lambda ()
                   (concat "Rename "
                           (propertize (copy-sequence (or
                                                       (and buffer-file-name
                                                            (abbreviate-file-name
                                                             buffer-file-name))
                                                       ""))
                                       'face 'transient-argument)))
    :inapt-if-not buffer-file-name)]
  ["Backup"
   ("b" km-buffer-make-backup
    :description
    (lambda ()
      (concat "Backup "
              (propertize
               (copy-sequence (or (and buffer-file-name
                                       (abbreviate-file-name
                                        buffer-file-name))
                                  ""))
               'face 'transient-argument))))
   ("f" "Find in backups" km-buffer-find-backup-file
    :inapt-if-not (lambda ()
                    (file-exists-p km-buffer-backup-directory)))
   ("d" "Jump to backup directory" km-buffer-find-backup-dir
    :inapt-if-not (lambda ()
                    (file-exists-p km-buffer-backup-directory)))
   ("m" "Find backup mirror" km-buffer-find-backup-mirror
    :inapt-if-not
    (lambda ()
      (when buffer-file-name
        (let ((dir (km-buffer-get-parent-target-dir)))
          (and (file-exists-p dir)
               (directory-files dir nil
                                (regexp-quote (file-name-nondirectory
                                               buffer-file-name))))))))]
  ["Misc"
   ("p" "Copy filename" km-buffer-kill-path-menu)
   ("s" "Open sqlite file" km-buffer-sqlite-open-file
    :if sqlite-available-p)])

(provide 'km-buffer)
;;; km-buffer.el ends here
