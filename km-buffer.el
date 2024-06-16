;;; km-buffer.el --- Additional buffer commands -*- lexical-binding: t -*-

;; Copyright © 2020-2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-buffer
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "29.1") (project "0.9.4") (transient "0.6.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; This package provides additional buffer commands for Emacs.
;;; Code:

(require 'transient)
(declare-function dired-get-marked-files "dired")
(declare-function dired-copy-file "dired-aux")


(defcustom km-buffer-backup-directory "~/.backups"
  "Directory to save backup files by `km-buffer-make-backup'."
  :type 'directory
  :group 'km-buffer)

(defcustom km-buffer-backup-time-format "%Y_%m_%d_%H_%M_%S"
  "Format given to `format-time-string' which is appended to the filename."
  :type 'directory
  :group 'km-buffer)


(defcustom km-buffer-extra-transient-suffixes '("Atomic chrome"
                                                ("A"
                                                 km-buffer-atomic-chrome-suffix-description
                                                 atomic-chrome-toggle-server
                                                 :if-require
                                                 (atomic-chrome)
                                                 :transient nil)
                                                ("."
                                                 km-buffer-atomic-chrome-selection-description
                                                 atomic-chrome-toggle-selection
                                                 :if-require
                                                 (atomic-chrome)
                                                 :if-non-nil
                                                 atomic-chrome-edit-mode
                                                 :transient t)
                                                "Pandoc"
                                                ("p" "Pandoc Menu"
                                                 pandoc-mini-menu :if-require
                                                 (pandoc-mini))
                                                ("o" "Pandoc Import to org"
                                                 org-pandoc-import-to-org
                                                 :if-require
                                                 (org-pandoc-import))
                                                "Other"
                                                ("C" "Chmod" chmod-menu
                                                 :if-require
                                                 (chmod-menu))
                                                ("i" "Show File Info"
                                                 file-info-show
                                                 :if-require
                                                 (file-info)
                                                 :inapt-if-nil buffer-file-name)
                                                ("g c"
                                                 "Show Commit Info at point"
                                                 blamer-show-commit-info
                                                 :if-require (blamer)
                                                 :inapt-if-not
                                                 km-buffer--git-exist-p)
                                                ("g m"
                                                 km-buffer-blamer-mode-description
                                                 blamer-mode
                                                 :if-require (blamer)
                                                 :inapt-if-nil buffer-file-name
                                                 :transient t))
  "Extra suffixes to add in `km-buffer-actions-menu'."
  :group 'km-buffer
  :type `(repeat
          (radio
           (string :tag "Title")
           (list
            :tag "Suffix"
            (string :tag "Key")
            (choice
             (string :tag "Description")
             (function :tag "Description Function")
             (sexp :tag "Description sexp"))
            (function :tag "Command")
            (set
             :tag "If require"
             :inline t
             (group
              :format "%v"
              :inline t
              (const
               :format ""
               :if-require)
              (repeat
               (symbol
                :tag "Library"
                :completions
                (lambda (string pred action)
                  (let ((completion-ignore-case t))
                   (complete-with-action action features string pred)))))))
            (repeat
             :tag "Inapt by modes"
             :inline t
             (list
              :tag "Inapt by modes"
              :inline t
              (radio
               (const
                :format "%v %d"
                :tag ":inapt-if-mode"
                :doc
                "Inapt if major-mode matches value."
                :inapt-if-mode)
               (const
                :format "%v %d"
                :tag ":inapt-if-not-mode"
                :doc
                "Inapt if major-mode does not match value."
                :inapt-if-not-mode)
               (const
                :format "%v %d"
                :tag ":inapt-if-derived"
                :doc
                "Inapt if major-mode derives from value."
                :inapt-if-derived)
               (const
                :format "%v %d"
                :tag ":inapt-if-not-derived"
                :doc
                "Inapt if major-mode does not derive from value."
                :inapt-if-not-derived))
              (symbol
               :completions
               (lambda (string pred action)
                 (let ((completion-ignore-case t))
                  (complete-with-action action
                   (remove 't
                    (seq-uniq
                     (seq-filter
                      #'symbolp
                      (flatten-list
                       auto-mode-alist))))
                   string pred))))))
            (repeat
             :tag "If mode"
             :inline t
             (list
              :inline t
              (radio
               (const
                :format "%v %d"
                :tag ":if-mode"
                :doc
                "Enable if major-mode matches value."
                :if-mode)
               (const
                :format "%v %d"
                :tag ":if-not-mode"
                :doc
                "Enable if major-mode does not match value."
                :if-not-mode)
               (const
                :format "%v %d"
                :tag ":if-derived"
                :doc
                "Enable if major-mode derives from value."
                :if-derived)
               (const
                :format "%v %d"
                :tag ":if-not-derived"
                :doc
                "Enable if major-mode does not derive from value."
                :if-not-derived))
              (symbol :completions
               (lambda (string pred action)
                 (let ((completion-ignore-case t))
                  (complete-with-action action
                   (remove 't
                    (seq-uniq
                     (seq-filter
                      #'symbolp
                      (flatten-list
                       auto-mode-alist))))
                   string pred))))))
            (repeat
             :inline t
             :tag "If variable"
             (list
              :inline t
              (radio
               (const
                :format "%v %d"
                :tag ":if-non-nil"
                :doc
                "Enable if variable's value is non-nil."
                :if-non-nil)
               (const
                :format "%v %d"
                :tag ":if-nil"
                :doc "Enable if variable's value is nil."
                :if-nil))
              variable))
            (repeat
             :inline t
             :tag "Inapt if variable"
             (list
              :inline t
              (radio
               (const
                :format "%v: %d"
                :tag ":inapt-if-non-nil"
                :doc
                "Inapt if variable's value is non-nil."
                :inapt-if-non-nil)
               (const
                :format "%v: %d"
                :tag ":inapt-if-nil"
                :doc
                "Inapt if variable's value is nil."
                :inapt-if-nil))
              variable))
            (repeat
             :tag "If predicate"
             :inline t
             (list
              :inline t
              (radio
               (const
                :format "%v %d"
                :tag ":if"
                :doc "Enable if predicate returns non-nil."
                :if)
               (const
                :format "%v %d"
                :tag ":if-not"
                :doc "Enable if predicate returns nil."
                :if-not))
              (choice (function :tag "Function")
               (symbol :tag "Symbol")
               (sexp :tag "Sexp"))))
            (repeat
             :inline t
             :tag "Inapt if predicate"
             (list
              :inline t
              (radio
               (const
                :format "%v %d"
                :tag ":inapt-if"
                :doc
                "Inapt if predicatea returns non-nil."
                :inapt-if)
               (const
                :format "%v %d"
                :tag ":inapt-if-not"
                :doc
                "Inapt if predicate returns nil."
                :inapt-if-not))
              (choice
               (function :tag "Function")
               (symbol :tag "Symbol")
               (sexp :tag "Sexp"))))
            (repeat
             :inline t
             (list
              :inline t
              (symbol :tag "other")
              (choice
               (function :tag "Function")
               (symbol :tag "Symbol")
               (sexp :tag "Sexp"))))))))

(defun km-buffer--get-local-name (filename)
  "Return local FILENAME if path is in the tramp format."
  (if (and filename (file-remote-p default-directory) filename
           (fboundp 'tramp-file-name-localname)
           (fboundp 'tramp-dissect-file-name))
      (tramp-file-name-localname (tramp-dissect-file-name filename))
    filename))


(defun km-buffer--git-exist-p ()
  "Return t if .git exist."
  (require 'vc-git)
  (when-let* ((file-name (km-buffer--get-local-name (buffer-file-name))))
    (vc-backend file-name)))


(defun km-buffer-blamer-mode-description ()
  "Return a string describing the status of `blamer-mode'."
  (string-join
   (list
    "Auto Show Commit Info"
    (and
     (bound-and-true-p blamer-mode)
     (propertize "(on)" 'face 'transient-value)))
   " "))


(defun km-buffer-atomic-chrome-suffix-description ()
  "Return a string describing the state of the atomic-chrome server."
  (string-join
   (list
    (if
        (bound-and-true-p
         atomic-chrome-server-atomic-chrome)
        "Stop" "Start")
    "server"
    (and
     (bound-and-true-p atomic-chrome-server-atomic-chrome)
     (propertize "(on)" 'face 'transient-value)))
   " "))

(defun km-buffer-atomic-chrome-selection-suffix-description ()
  "Return a string describing the state of selection synchronization."
  (string-join
   (list
    (if
        (bound-and-true-p
         atomic-chrome-max-text-size-for-selection-sync)
        "Disable" "Enable")
    "selection synchronization"
    (propertize
     (format "(%s)"
             (and
              (bound-and-true-p
               atomic-chrome-max-text-size-for-selection-sync)
              atomic-chrome-max-text-size-for-selection-sync))
     'face 'transient-value))
   " "))

;;;###autoload (autoload 'km-buffer-pandoc-import-transient "km-buffer" nil t)
(transient-define-prefix km-buffer-pandoc-import-transient ()
  "Command dispatcher for `org-pandoc-import'."
  [:if (lambda ()
         (require 'org-pandoc-import nil t)
         (featurep 'org-pandoc-import))
       [("t"
         org-pandoc-import-transient-mode
         :description (lambda ()
                        (concat "Import transient mode "
                                (if
                                    (bound-and-true-p
                                     org-pandoc-import-transient-mode)
                                    (propertize
                                     "(on) "
                                     'face
                                     'success)
                                  (propertize
                                   "(off) "
                                   'face
                                   'error)))))
        ("o" "to org" org-pandoc-import-to-org)
        ("a" "as org" org-pandoc-import-as-org)
        ("ca" "csv-as-org" org-pandoc-import-csv-as-org)
        ("co" "csv-to-org" org-pandoc-import-csv-to-org)
        ("da" "odt-as-org" org-pandoc-import-odt-as-org)
        ("do" "odt-to-org" org-pandoc-import-odt-to-org)
        ("ma" "markdown-as-org" org-pandoc-import-markdown-as-org)
        ("md" "markdown-to-org" org-pandoc-import-markdown-to-org)
        ("rd" "rmarkdown-as-org" org-pandoc-import-rmarkdown-as-org)
        ("rm" "rmarkdown-to-org" org-pandoc-import-rmarkdown-to-org)]
       [("ro" "rst-to-org" org-pandoc-import-rst-to-org)
        ("va" "tsv-as-org" org-pandoc-import-tsv-as-org)
        ("vo" "tsv-to-org" org-pandoc-import-tsv-to-org)
        ("xa" "docx-as-org" org-pandoc-import-docx-as-org)
        ("xo" "docx-to-org" org-pandoc-import-docx-to-org)
        ("ia" "ipynb-as-org" org-pandoc-import-ipynb-as-org)
        ("io" "ipynb-to-org" org-pandoc-import-ipynb-to-org)
        ("la" "latex-as-org" org-pandoc-import-latex-as-org)
        ("lo" "latex-to-org" org-pandoc-import-latex-to-org)
        ("ra" "rst-as-org" org-pandoc-import-rst-as-org)]])

;;;###autoload
(defun km-buffer-delete-current-buffer-file ()
  "Remove file and kill current buffer without prompt."
  (interactive)
  (let* ((buffer (current-buffer))
         (filename (buffer-file-name buffer)))
    (when (and buffer (buffer-live-p buffer))
      (when filename
        (save-buffer)
        (km-buffer-make-backup))
      (kill-buffer buffer))
    (when filename
      (delete-file filename t))))


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
  (let ((backup-container
         (format "%s/%s"
                 km-buffer-backup-directory
                 default-directory)))
    (unless (or dont-create
                (file-exists-p backup-container))
      (make-directory backup-container t))
    backup-container))

(defun km-buffer-backup-directory-exists-p ()
  "Return non-nil whether `km-buffer-backup-directory' is existing directory."
  (and km-buffer-backup-directory
       (file-exists-p km-buffer-backup-directory)))

(defvar km-buffer-minibuffer-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j")
                #'km-buffer-minibuffer-preview-file)
    (define-key map (kbd "C-c C-o")
                #'km-buffer-minibuffer-find-file-other-window-and-exit)
    map))


;;;###autoload
(defun km-buffer-find-backup-file ()
  "Find a file in `km-buffer-backup-directory'."
  (interactive)
  (if (km-buffer-backup-directory-exists-p)
      (find-file
       (km-buffer-completing-read-with-keymap
        "File: "
        (directory-files-recursively
         km-buffer-backup-directory
         directory-files-no-dot-files-regexp
         nil)
        km-buffer-minibuffer-file-map))
    (message "Directory %s doesn't exist"
             km-buffer-backup-directory)))


;;;###autoload
(defun km-buffer-find-backup-dir ()
  "Find a backup directory `km-buffer-backup-directory'."
  (interactive)
  (if (km-buffer-backup-directory-exists-p)
      (find-file km-buffer-backup-directory)
    (message "Directory %s doesn't exist"
             km-buffer-backup-directory)))

(defun km-buffer-get-empty-dirs-or-subdirs (directory)
  "Search for empty directories or subdirectories in DIRECTORY."
  (let ((result))
    (dolist (file (directory-files directory t
                                   directory-files-no-dot-files-regexp))
      (when (and (file-directory-p file)
                 (km-buffer-is-dir-empty-p file))
        (push file result)))
    result))

(defun km-buffer-is-dir-empty-p (file)
  "Return t if FILE is an empty directory or a directory with empty subdirs."
  (cond ((not (file-directory-p file))
         nil)
        (t
         (or (directory-empty-p file)
             (seq-every-p
              (lambda (f)
                (km-buffer-is-dir-empty-p f))
              (directory-files
               file t
               directory-files-no-dot-files-regexp))))))


;;;###autoload
(defun km-buffer-remove-empty-dirs ()
  "Query remove empty directories or subdirectories in default directory."
  (interactive)
  (let ((dirs (km-buffer-get-empty-dirs-or-subdirs default-directory))
        (dir)
        (all))
    (while (setq dir (pop dirs))
      (when (and (derived-mode-p 'dired-revert)
                 (fboundp 'dired-revert))
        (dired-revert nil nil))
      (if all
          (delete-directory dir t)
        (pcase
            (car (read-multiple-choice
                  (if (directory-empty-p dir)
                      (format "Remove directory %s?" dir)
                    (format "Remove directory %s with all subdirectories?" dir))
                  `((?y "yes")
                    (?n "no")
                    (?! ,(format "Delete all remaining %s dirs" (length dirs)))
                    (?q "quit"))))
          (?y (delete-directory dir t))
          (?! (delete-directory dir t)
              (setq all t)))))
    (when (and (derived-mode-p 'dired-revert)
               (fboundp 'dired-revert))
      (dired-revert nil nil))))

(defun km-buffer-get-mirrored-files (file)
  "Return backuped FILE versions."
  (when (and file)
    (let ((default-directory
           (if (file-directory-p file)
               file
             (file-name-parent-directory file)))
          (regex (regexp-quote
                  (if (file-directory-p file)
                      (file-name-nondirectory (directory-file-name
                                               file))
                    (file-name-nondirectory file)))))
      (delete nil
              (seq-uniq
               (mapcan (lambda (it)
                         (when (file-exists-p it)
                           (if (file-directory-p it)
                               (directory-files it t regex)
                             (list it))))
                       (when-let ((parent (km-buffer-get-parent-target-dir t)))
                         (let ((top-parent (ignore-errors
                                             (file-name-parent-directory
                                              parent))))
                           (delq nil
                                 (seq-uniq
                                  (append
                                   (and parent
                                        (file-exists-p parent)
                                        (directory-files parent t
                                                         (regexp-quote
                                                          (file-name-nondirectory
                                                           (directory-file-name
                                                            parent)))))
                                   (and top-parent
                                        (file-exists-p top-parent)
                                        (directory-files top-parent t
                                                         (regexp-quote
                                                          (file-name-nondirectory
                                                           (directory-file-name
                                                            parent))))))))))))))))
;;;###autoload
(defun km-buffer-find-backup-mirror ()
  "Find backup file for current file."
  (interactive)
  (let* ((containing-dir default-directory)
         (backup-container
          (format "%s/%s"
                  km-buffer-backup-directory
                  containing-dir)))
    (if (not (file-exists-p backup-container))
        (message "Directory %s doesn't exist"
                 km-buffer-backup-directory)
      (find-file
       (km-buffer-completing-read-with-keymap
        "File: "
        (km-buffer-get-mirrored-files (or buffer-file-name default-directory))
        km-buffer-minibuffer-file-map)))))

(defun km-buffer-file-name-split (filename)
  "Return a list of all the components of FILENAME.
On most systems, this will be true:

  (equal (string-join (file-name-split filename) \"/\") filename)"
  (if (fboundp 'file-name-split)
      (file-name-split filename)
    (let ((components nil))
    ;; If this is a directory file name, then we have a null file name
    ;; at the end.
      (when (directory-name-p filename)
        (push "" components)
        (setq filename (directory-file-name filename)))
        ;; Loop, chopping off components.
      (while (length> filename 0)
        (push (file-name-nondirectory filename) components)
        (let ((dir (file-name-directory filename)))
          (setq filename (and dir (directory-file-name dir)))
          ;; If there's nothing left to peel off, we're at the root and
          ;; we can stop.
          (when (and dir (equal dir filename))
            (push (if (equal dir "") ""
            ;; On Windows, the first component might be "c:" or
            ;; the like.
                    (substring dir 0 -1))
                  components)
            (setq filename nil))))
      components)))

;;;###autoload
(defun km-buffer-backup-marked-files ()
  "Make a backup copy of current file or `dired' marked files.
If in `dired', backup current file or marked files.
See also `km-buffer-backup-time-format'."
  (interactive)
  (let ((alist (mapcar (lambda (x)
                         (let ((backup-name
                                (if (not (file-directory-p x))
                                    (expand-file-name
                                     (concat
                                      (file-name-nondirectory
                                       x)
                                      "~"
                                      (format-time-string
                                       km-buffer-backup-time-format))
                                     (km-buffer-get-parent-target-dir))
                                  (concat
                                   (expand-file-name
                                    (car
                                     (seq-drop-while
                                      #'string-empty-p
                                      (reverse
                                       (km-buffer-file-name-split
                                        x))))
                                    (km-buffer-get-parent-target-dir))
                                   "~"
                                   (format-time-string
                                    km-buffer-backup-time-format)))))
                           (cons x backup-name)))
                       (seq-remove (lambda (it)
                                     (or (string-suffix-p "./" it)
                                         (string-suffix-p "../" it)))
                                   (dired-get-marked-files)))))
    (dolist (cell alist)
      (message "Copying %s to %s" (car cell)
               (cdr cell))
      (dired-copy-file (car cell)
                       (cdr cell)
                       1))))

;;;###autoload
(defun km-buffer-make-backup ()
  "Make a backup copy `dired' marked files.
See also `km-buffer-backup-time-format'."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (km-buffer-backup-marked-files)
    (let ((new-file-name-base))
      (when buffer-file-name
        (setq new-file-name-base (concat (file-name-nondirectory
                                          buffer-file-name)
                                         "~"
                                         (format-time-string
                                          km-buffer-backup-time-format))))
      (when (and km-buffer-backup-directory
                 (not (km-buffer-backup-directory-exists-p)))
        (make-directory km-buffer-backup-directory t))
      (when-let ((dir (and new-file-name-base
                           (km-buffer-get-parent-target-dir))))
        (let ((backup-name
               (expand-file-name new-file-name-base dir)))
          (unless (file-exists-p dir)
            (make-directory dir t))
          (copy-file buffer-file-name backup-name t)
          (message " %s"(concat "Backup saved at: "
                                backup-name)))))))

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
  (when (fboundp 'sqlite-mode-open-file)
    (if (and buffer-file-name
             (member (file-name-extension buffer-file-name) '("sqlite" "db")))
        (sqlite-mode-open-file buffer-file-name)
      (read-file-name "SQLite file name: "
                      (when buffer-file-name
                        (file-name-nondirectory
                         buffer-file-name))))))

(defun km-buffer-minibuffer-get-metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun km-buffer-minibuffer-ivy-selected-cand ()
  "Return the currently selected item in Ivy."
  (when (and (memq 'ivy--queue-exhibit post-command-hook)
             (boundp 'ivy-text)
             (boundp 'ivy--length)
             (boundp 'ivy-last)
             (fboundp 'ivy--expand-file-name)
             (fboundp 'ivy-state-current))
    (cons
     (completion-metadata-get (ignore-errors (km-buffer-minibuffer-get-metadata))
                              'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun km-buffer-minibuffer-get-default-candidates ()
  "Return all current completion candidates from the minibuffer."
  (when (minibufferp)
    (let* ((all (completion-all-completions
                 (minibuffer-contents)
                 minibuffer-completion-table
                 minibuffer-completion-predicate
                 (max 0 (- (point)
                           (minibuffer-prompt-end)))))
           (last (last all)))
      (when last (setcdr last nil))
      (cons
       (completion-metadata-get (km-buffer-minibuffer-get-metadata) 'category)
       all))))

(defun km-buffer-get-default-top-minibuffer-completion ()
  "Target the top completion candidate in the minibuffer.
Return the category metadatum as the type of the target."
  (when (and (minibufferp) minibuffer-completion-table)
    (pcase-let* ((`(,category . ,candidates)
                  (km-buffer-minibuffer-get-default-candidates))
                 (contents (minibuffer-contents))
                 (top (if (test-completion contents
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate)
                          contents
                        (let ((completions (completion-all-sorted-completions)))
                          (if (null completions)
                              contents
                            (concat
                             (substring contents
                                        0 (or (cdr (last completions)) 0))
                             (car completions)))))))
      (cons category (or (car (member top candidates)) top)))))

(defvar km-buffer-minibuffer-targets-finders
  '(km-buffer-minibuffer-ivy-selected-cand
    km-buffer-get-default-top-minibuffer-completion))

(defun km-buffer-minibuffer-get-current-candidate ()
  "Return cons filename for current completion candidate."
  (let (target)
    (run-hook-wrapped
     'km-buffer-minibuffer-targets-finders
     (lambda (fun)
       (when-let ((result (funcall fun)))
         (when (and (cdr-safe result)
                    (stringp (cdr-safe result))
                    (not (string-empty-p (cdr-safe result))))
           (setq target result)))
       (and target (minibufferp))))
    target))

(defun km-buffer-minibuffer-exit-with-action (action)
  "Call ACTION with current candidate and exit minibuffer."
  (pcase-let ((`(,_category . ,current)
               (km-buffer-minibuffer-get-current-candidate)))
    (progn (run-with-timer 0.1 nil action current)
           (abort-minibuffers))))

(defun km-buffer-minibuffer-web-restore-completions-wind ()
  "Restore *Completions* window height."
  (when-let ((win (get-buffer-window "*Completions*" 0)))
    (when (and (boundp 'completions-max-height)
               (numberp completions-max-height))
      (fit-window-to-buffer win completions-max-height))))

(defun km-buffer-minibuffer-action-no-exit (action)
  "Call ACTION with minibuffer candidate in its original window."
  (remove-hook 'pre-command-hook
            #'km-buffer-minibuffer-web-restore-completions-wind)
  (pcase-let ((`(,_category . ,current)
               (km-buffer-minibuffer-get-current-candidate)))
    (with-minibuffer-selected-window
      (funcall action current))))

;;;###autoload
(defun km-buffer-minibuffer-find-file-other-window-and-exit ()
  "Exit minibuffer and file selected candidate as file in other window."
  (interactive)
  (km-buffer-minibuffer-exit-with-action #'find-file-other-window))

;;;###autoload
(defun km-buffer-minibuffer-preview-file ()
  "Preview current minibuffer file."
  (interactive)
  (km-buffer-minibuffer-action-no-exit #'find-file))


(defun km-buffer-completing-read-with-keymap (prompt collection &optional keymap
                                                     predicate require-match
                                                     initial-input hist def
                                                     inherit-input-method)
  "Read COLLECTION in minibuffer with PROMPT and KEYMAP.
See `completing-read' for PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD."
  (let ((collection (if (stringp (car-safe collection))
                        (copy-tree collection)
                      collection)))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (minibufferp)
            (when keymap
              (let ((map (make-composed-keymap keymap
                                               (current-local-map))))
                (use-local-map map)))))
      (completing-read prompt
                       collection
                       predicate
                       require-match initial-input hist
                       def inherit-input-method))))

;;;###autoload
(defun km-buffer-get-active-buffers-dirs ()
  "Return list of uniq default directories from all buffers."
  (let* ((curr-buf (current-buffer))
         (live-buffers (seq-sort-by (lambda (it)
                                      (if (get-buffer-window it)
                                          (if (eq curr-buf it)
                                              1
                                            2)
                                        -1))
                                    #'>
                                    (buffer-list))))
    (delete-dups
     (remove nil (mapcar (lambda (buff)
                           (when-let ((dir (buffer-local-value
                                            'default-directory
                                            buff)))
                             (unless (file-remote-p dir)
                               (expand-file-name dir))))
                         live-buffers)))))

(defun km-buffer-read-active-buffers-directory (prompt)
  "Read directory name with PROMPT and completing directories from all buffers."
  (km-buffer-completing-read-with-keymap
   prompt
   (km-buffer-get-active-buffers-dirs)
   km-buffer-minibuffer-file-map))

(defun km-buffer-f-read-dir (prompt)
  "Read CHOICES with PROMPT and ARGS with completion."
  (require 'multi-source nil t)
  (if (fboundp 'multi-source-read)
      (multi-source-read `(("Directory" km-buffer-read-active-buffers-directory
                            ,prompt)
                           ("Other directory" read-directory-name ,prompt)))
    (km-buffer-read-active-buffers-directory prompt)))

;;;###autoload
(defun km-buffer-copy-file (&optional source-file target-directory)
  "Copy SOURCE-FILE to TARGET-DIRECTORY.
SOURCE-FILE can be also list of files to copy."
  (interactive (list
                (if-let* ((url
                           (when (derived-mode-p 'xwidget-webkit-mode)
                             (xwidget-webkit-uri
                              (when (fboundp
                                     'xwidget-webkit-current-session)
                                (xwidget-webkit-current-session)))))
                          (file
                           (when (string-prefix-p
                                  "file:///"
                                  url)
                             (replace-regexp-in-string
                              "^file:///" "" url)))
                          (dir (file-name-directory file)))
                    (read-file-name "File to copy: " dir
                                    nil
                                    t
                                    (file-name-nondirectory
                                     file))
                  (read-file-name "File to copy: " nil nil t
                                  (when buffer-file-name
                                    (file-name-nondirectory
                                     buffer-file-name))))
                (km-buffer-f-read-dir "Copy to: ")))
  (require 'dired)
  (dired-copy-file source-file target-directory 1))

(defun km-buffer-view-echo-messages-0 ()
  "Display the *Messages* buffer, scrolling to the latest message."
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

;;;###autoload (autoload 'km-buffer-kill-path-menu "km-buffer" nil t)
(transient-define-prefix km-buffer-kill-path-menu ()
  "Command dispatcher for copying current buffer file name in misc formats."
  ["Copy filename"
   ("w" km-buffer-kill-absolute-filename
    :description (lambda ()
                   (concat (or buffer-file-name
                               "")))
    :inapt-if-not buffer-file-name)
   ("a" km-buffer-kill-absolute-filename-abbreviated
    :description (lambda ()
                   (concat (or (and buffer-file-name
                                    (abbreviate-file-name buffer-file-name))
                               "")))
    :inapt-if-not (lambda ()
                    (and buffer-file-name
                         (abbreviate-file-name buffer-file-name))))]
  ["Copy directory"
   ("d" km-buffer-kill-directory
    :description (lambda ()
                   (concat (or (if buffer-file-name
                                   (file-name-directory buffer-file-name)
                                 default-directory)
                               "")))
    :inapt-if-not (lambda ()
                    (if buffer-file-name
                        (file-name-directory buffer-file-name)
                      default-directory)))
   ("D" km-buffer-kill-directory-abbreviated
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
   ("p" km-buffer-kill-current-file-relative-project-name
    :description (lambda ()
                   (or (km-buffer-current-file-relative-project-name)
                       "No project"))
    :inapt-if-not km-buffer-current-file-relative-project-name
    :if (lambda ()
          (not (equal (km-buffer-file-name--nondirectory)
                      (km-buffer-current-file-relative-project-name)))))
   ("n" km-buffer-kill-file-name-nondirectory
    :description (lambda ()
                   (concat (or (km-buffer-file-name--nondirectory)
                               "")))
    :inapt-if-not km-buffer-file-name--nondirectory)
   ("b" km-buffer-kill-current-filename-base
    :description (lambda ()
                   (concat (or (km-buffer-current-file--basename)
                               "")))
    :inapt-if-not buffer-file-name)])

(defun km-buffer-menu-make-file-action-description (label)
  "Return LABEL concatenated with fontified filename of the current buffer."
  (if buffer-file-name
      (concat label
              " ("
              (file-name-nondirectory
               (directory-file-name
                (abbreviate-file-name
                 buffer-file-name)))
              ")")
    label))


(defun km-buffer-get-emacs-source-mirror-file (file)
  "Retrieve corresponding source file for FILE from Emacs `data-directory'.

Argument FILE is the name of the file for which to find the Emacs source mirror
file."
  (when (and
         file
         (bound-and-true-p source-directory)
         (bound-and-true-p data-directory)
         (file-accessible-directory-p source-directory)
         (file-accessible-directory-p data-directory))
    (let* ((data-parent-dir (file-name-parent-directory data-directory))
           (source-file
            (when (file-in-directory-p
                   file
                   data-parent-dir)
              (expand-file-name
               (substring-no-properties
                (expand-file-name file)
                (length
                 data-parent-dir))
               source-directory))))
      (when (and source-file
                 (file-exists-p source-file))
        source-file))))

;;;###autoload
(defun km-buffer-find-emacs-related-source-file ()
  "Locate and open the source file associated with the current buffer."
  (interactive)
  (when-let ((source-file (km-buffer-get-emacs-source-mirror-file
                           buffer-file-name))
             (pos (point)))
    (find-file source-file)
    (goto-char pos)))

(defun km-buffer--is-all-libs-loadable (libs)
  "Check if all libraries in LIBS are loadable without errors.

Argument LIBS is a list of symbols representing libraries to check for
loadability."
  (not (catch 'found
         (dolist (sym libs)
           (unless
               (require sym nil t)
             (throw 'found t))))))

(defun km-buffer--plist-remove (keys plist)
  "Remove KEYS and values from PLIST."
  (let* ((result (list 'head))
         (last result))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (new (and (not (memq key keys))
                       (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (cdr result)))

(defun km-buffer--check-extra-suffix (spec)
  "Check if SPEC is a string or meets certain conditions for loading.

Argument SPEC is a string or a list where the third element is a function to
check for loadability and the remaining elements specify libraries to load."
  (or (stringp spec)
      (let ((props (seq-drop spec 3)))
        (and
         (km-buffer--is-all-libs-loadable
          (plist-get props :if-require))
         (fboundp (caddr spec))))))

(defun km-buffer--normalize-suffix-spec (spec)
  "Normalize suffix specification SPEC into a consistent format.

Argument SPEC is a string or a list containing a key, a description, a command,
and a property list."
  (if (stringp spec)
      spec
    (pcase-let* ((`(,key ,description ,cmd . ,plist)
                  spec)
                 (suff (if (stringp description)
                           (list key description cmd)
                         (list key
                               cmd
                               :description
                               description)))
                 (props (km-buffer--plist-remove
                         '(:if-require)
                         plist)))
      (append suff props))))

(defvar km-buffer--extra-commands nil)

(defun km-buffer--group-list-by-headers (input-list)
  "Groups elements in INPUT-LIST by headers (strings) into sublists."
  (let ((result '())
        (current-group '()))
    (dolist (elem input-list)
      (if (stringp elem)
          (progn
            ;; When encountering a header, first check if the current-group
            ;; has items. If it does, add it to the result and start a new group.
            (when current-group
              (push (nreverse current-group) result)
              (setq current-group '())) ;; Reset current group.
            ;; Start a new group with the header as the first element.
            (push elem current-group))
        ;; If the element is not a header, add it to the current group.
        (push elem current-group)))
    ;; After looping, add the last group to the result if it's not empty.
    (when current-group
      (push (nreverse current-group) result))
    (nreverse result)))


(defun km-buffer--get-extra-suffixes ()
  "Filter and group extra suffixes based on specific conditions."
  (let ((all-items (mapcar #'km-buffer--normalize-suffix-spec
                           (seq-filter #'km-buffer--check-extra-suffix
                                       km-buffer-extra-transient-suffixes))))
    (km-buffer--group-list-by-headers all-items)))

(defun km-buffer-make-file-executable-if-script-p-after-save ()
  "Enable make file executable according to umask if not already executable."
  (require 'executable)
  (add-hook 'after-save-hook
            #'executable-make-buffer-file-executable-if-script-p
            nil t))


;;;###autoload (autoload 'km-buffer-actions-menu "km-buffer" nil t)
(transient-define-prefix km-buffer-actions-menu ()
  "Command dispatcher with buffer commands."
  [:description
   (lambda ()
     (concat (propertize
              (format
               "%s"
               (when default-directory
                 (abbreviate-file-name default-directory)))
              'face `(:inherit
                      font-lock-constant-face)
              'display '((height 1.1)))
             (propertize "\n" 'face
                         'font-lock-doc-face)))
   [:description
    (lambda ()
      (km-buffer-menu-make-file-action-description "Act on file"))
    ("r" "Reload"
     km-buffer-reload-current-buffer
     :inapt-if-not buffer-file-name)
    ("D" "Delete" km-buffer-delete-current-buffer-file
     :inapt-if-not buffer-file-name)
    ("R" "Rename" km-buffer-rename-current-buffer-file
     :inapt-if-not buffer-file-name)
    ("c" "Copy" km-buffer-copy-file)
    ("b" km-buffer-make-backup
     :description
     (lambda ()
       (concat "Backup"
               (if (derived-mode-p
                    'dired-mode)
                   (let* ((marked (dired-get-marked-files))
                          (len (length marked))
                          (desc (mapcar
                                 (lambda (f)
                                   (file-name-nondirectory
                                    (directory-file-name f)))
                                 marked)))
                     (format " %d files %s" len
                             (if desc
                                 (format "(%s)"
                                         (propertize
                                          (truncate-string-to-width
                                           (string-join desc
                                                        ", ")
                                           20 nil nil "...")
                                          'face
                                          'transient-argument))
                               "")))
                 ""))))
    ("S" "Sudo edit" sudo-edit
     :if (lambda ()
           (require 'sudo-edit nil t)))
    ("t" "Sudo other file"
     sudo-edit-find-file
     :if (lambda ()
           (require 'sudo-edit nil t)))]
   ["Backup"
    ("F" "Find in backups" km-buffer-find-backup-file
     :inapt-if-not km-buffer-backup-directory-exists-p)
    ("d" "Jump to backup directory" km-buffer-find-backup-dir
     :inapt-if-not km-buffer-backup-directory-exists-p)
    ("m" "Find backup mirror" km-buffer-find-backup-mirror
     :inapt-if-not
     (lambda ()
       (km-buffer-get-mirrored-files buffer-file-name)))
    ("j" "Find emacs file source mirror"
     km-buffer-find-emacs-related-source-file
     :if
     (lambda ()
       (km-buffer-get-emacs-source-mirror-file buffer-file-name)))]]
  [["Act on buffer"
    :description
    (lambda ()
      (km-buffer-menu-make-file-action-description "Act on buffer"))
    ("G" "Revert" revert-buffer :inapt-if-not
     buffer-modified-p)
    ("k" "Kill" kill-current-buffer)]
   ["Misc"
    ("w" "Copy filepath" km-buffer-kill-path-menu)
    ("s" "Open sqlite file" km-buffer-sqlite-open-file
     :if sqlite-available-p)
    ("I" "Pandoc Import Menu" km-buffer-pandoc-import-transient
     :if (lambda ()
           (require 'org-pandoc-import nil t)
           (featurep 'org-pandoc-import)))]]
  [[:setup-children
    (lambda (_args)
      (transient-parse-suffixes
       (oref transient--prefix command)
       (nth 0 km-buffer--extra-commands)))
    :class transient-column]
   [:setup-children
    (lambda (_args)
      (transient-parse-suffixes
       (oref transient--prefix command)
       (nth 1 km-buffer--extra-commands)))
    :class transient-column]]
  [[:setup-children
    (lambda (_args)
      (transient-parse-suffixes
       (oref transient--prefix command)
       (nth 2 km-buffer--extra-commands)))
    :class transient-column]
   [:setup-children
    (lambda (_args)
      (transient-parse-suffixes
       (oref transient--prefix command)
       (nth 3 km-buffer--extra-commands)))
    :class transient-column]]
  [[:setup-children
    (lambda (_args)
      (transient-parse-suffixes
       (oref transient--prefix command)
       (nth 4 km-buffer--extra-commands)))
    :class transient-column]
   [:setup-children
    (lambda (_args)
      (transient-parse-suffixes
       (oref transient--prefix command)
       (nth 5 km-buffer--extra-commands)))
    :class transient-column]]
  (interactive)
  (setq km-buffer--extra-commands (km-buffer--get-extra-suffixes))
  (transient-setup #'km-buffer-actions-menu))

(provide 'km-buffer)
;;; km-buffer.el ends here
