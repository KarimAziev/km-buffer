;;; km-buffer.el --- Configure buffer -*- lexical-binding: t -*-

;; Copyright © 2020-2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-buffer
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "28.1") (project "0.9.4"))
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

;; This file configures operations with buffer

;;; Code:

(require 'transient)
(declare-function dired-get-marked-files "dired")
(declare-function dired-copy-file "dired-aux")


(defcustom km-buffer-backup-directory "~/.backups"
  "Directory to save backup files by `km-buffer-make-backup'."
  :type 'directory
  :group 'km)

(defcustom km-buffer-backup-time-format "%Y_%m_%d_%H_%M_%S"
  "Format given to `format-time-string' which is appended to the filename."
  :type 'directory
  :group 'km)

(defcustom km-buffer-extra-transient-suffixes '(("p" "Pandoc Menu"
                                                 pandoc-mini-menu)
                                                ("o" "Pandoc Import to org"
                                                 org-pandoc-import-to-org)
                                                ("t" "Sudo Edit File"
                                                 sudo-edit-find-file)
                                                ("S" "Sudo Edit This File"
                                                 sudo-edit)
                                                ("C" "Chmod" chmod-menu)
                                                ("i" "File Info" file-info-show))
  "Extra suffixes to add in `km-buffer-actions-menu'."
  :group 'sisyphus
  :type '(repeat (list :tag "Suffix"
                       (string :tag "Key")
                       (string :tag "Description")
                       (function :tag "Command"))))

;;;###autoload (autoload 'km-buffer-pandoc-import-transient "km-buffer.el" nil t)
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


;;;###autoload
(defun km-buffer-find-backup-file ()
  "Find a file in `km-buffer-backup-directory'."
  (interactive)
  (if (km-buffer-backup-directory-exists-p)
      (completing-read "File: "
                       (directory-files-recursively
                        km-buffer-backup-directory
                        directory-files-no-dot-files-regexp
                        nil))
    (message "Directory %s doesn't exist"
             km-buffer-backup-directory)))


;;;###autoload
(defun km-buffer-find-backup-dir ()
  "Find a backup mirror of current file."
  (interactive)
  (if (km-buffer-backup-directory-exists-p)
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
                                       (file-name-split
                                        x))))
                                    (km-buffer-get-parent-target-dir))
                                   "~"
                                   (format-time-string
                                    km-buffer-backup-time-format)))))
                           (cons x backup-name)))
                       (dired-get-marked-files))))
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
  (sqlite-mode-open-file
   (read-file-name "SQLite file name: "
                   (when buffer-file-name
                     (file-name-nondirectory
                      buffer-file-name)))))

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
  (when (eq this-command 'minibuffer-next-completion)
    (remove-hook 'post-command-hook
                 #'km-buffer-minibuffer-web-restore-completions-wind)
    (when-let ((win (get-buffer-window "*Completions*" 0)))
      (fit-window-to-buffer win completions-max-height))))

(defun km-buffer-minibuffer-action-no-exit (action)
  "Call ACTION with minibuffer candidate in its original window."
  (pcase-let ((`(,_category . ,current)
               (km-buffer-minibuffer-get-current-candidate)))
    (when-let ((win (get-buffer-window "*Completions*" 0)))
      (minimize-window win)
      (add-hook 'post-command-hook
                #'km-buffer-minibuffer-web-restore-completions-wind))
    (with-minibuffer-selected-window
      (funcall action current))))

;;;###autoload
(defun km-buffer-minibuffer-preview-file ()
  "Call ACTION with minibuffer candidate in its original window."
  (interactive)
  (km-buffer-minibuffer-action-no-exit 'find-file))

(defvar km-buffer-minibuffer-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j")
                #'km-buffer-minibuffer-preview-file)
    map))

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
  (concat label
          " "
          (or
           (if buffer-file-name
               (propertize (abbreviate-file-name
                            buffer-file-name)
                           'face 'transient-argument))
           "")))

(defun km-buffer-transient-suffixes-watcher (_symbol newval _operation _buffer)
  "Variable watcher to update transient prefix `magit-tag' with NEWVAL."
  (let* ((layout (get 'km-buffer-actions-menu 'transient--layout))
         (len (length layout)))
    (when-let ((suffix-idx (catch 'found
                             (dotimes (idx len)
                               (let ((group (nth idx layout)))
                                 (when (and (vectorp group)
                                            (eq :description
                                                (car-safe (aref group 2)))
                                            (equal
                                             (cadr (aref group 2))
                                             "Sisyphus"))
                                   (throw 'found idx)))))))
      (transient-remove-suffix 'magit-tag (list suffix-idx))))
  (when newval
    (transient-append-suffix 'magit-tag
      (list
       (let ((i (1- (length (get 'magit-tag 'transient--layout)))))
         (if (>= i 0)
             i
           0)))
      (apply #'vector
             (append
              (list "Sisyphus")
              newval)))))

;;;###autoload (autoload 'km-buffer-actions-menu "km-buffer.el" nil t)
(transient-define-prefix km-buffer-actions-menu ()
  "Command dispatcher with buffer commands."
  ["Buffer actions"
   ("G" "Revert buffer" revert-buffer :inapt-if-not
    buffer-modified-p)
   ("k" "Kill current buffer" kill-current-buffer)]
  [:description
   "File actions\n"
   ("r" km-buffer-reload-current-buffer
    :description (lambda ()
                   (km-buffer-menu-make-file-action-description "Reload"))
    :inapt-if-not buffer-file-name)
   ("D"
    km-buffer-delete-current-buffer-file
    :description (lambda ()
                   (km-buffer-menu-make-file-action-description "Delete"))
    :inapt-if-not buffer-file-name)
   ("R" km-buffer-rename-current-buffer-file
    :description (lambda ()
                   (km-buffer-menu-make-file-action-description "Rename"))
    :inapt-if-not buffer-file-name)
   ("c" "Copy" km-buffer-copy-file)]
  ["Backup"
   ("b" km-buffer-make-backup
    :description
    (lambda ()
      (km-buffer-menu-make-file-action-description "Backup")))
   ("F" "Find in backups" km-buffer-find-backup-file
    :inapt-if-not km-buffer-backup-directory-exists-p)
   ("d" "Jump to backup directory" km-buffer-find-backup-dir
    :inapt-if-not km-buffer-backup-directory-exists-p)
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
   ("w" "Copy filename" km-buffer-kill-path-menu)
   ("s" "Open sqlite file" km-buffer-sqlite-open-file
    :if sqlite-available-p)
   ("I" "Pandoc Import Menu" km-buffer-pandoc-import-transient
    :if (lambda ()
          (require 'org-pandoc-import nil t)
          (featurep 'org-pandoc-import)))]
  [:setup-children
   (lambda (_args)
     (transient-parse-suffixes
      transient--prefix
      (apply #'vector
             (seq-filter (lambda (l)
                           (fboundp (car (last l))))
                         km-buffer-extra-transient-suffixes))))])

(provide 'km-buffer)
;;; km-buffer.el ends here
