;;; org-z.el --- Lightweight, Org-mode flavored zettelkasten links -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020 Mark Hudnall <me@markhudnall.com>

;; Author: Mark Hudnall <me@markhudnall.com>
;; URL: https://github.com/landakram/org-z
;; Keywords: org-mode
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (org "9.3") (helm-org-rifle "1.7.1") (helm-rg "0.1") (dash "2.12") (f "0.18.1") (s "1.10.0"))
;; Keywords: org-mode, outlines

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Utilities for linking in org-mode

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-capture)
(require 'helm-org-rifle)
(require 'helm-rg)
(require 'dash)
(require 'f)
(require 's)

(defgroup org-z nil
  "org-z customizable variables."
  :group 'org)

(defcustom org-z-directories (list org-directory)
  "Directories in which org-z will look for org files."
  :type 'list
  :group 'org-z)

(defcustom org-z-new-headings-file (f-join org-directory "new.org")
  "File in which to write new headings when inserting a link to a heading that does not already exist."
  :type 'file
  :group 'org-z)

(defun org-z-capture--templates (heading)
  "The capture templates used by org-z to create a heading when inserting a link to a heading that doesn't exist."
  (let ((body (concat "* " heading "%?\n:LOGBOOK:\n- Added %U\n:END:\n")))
    `(("d" "default" entry (file org-z-new-headings-file)
       ,body
       :immediate-finish t))))

(defcustom org-z-capture-templates #'org-z-capture--templates
  "The capture templates used by org-z to create a heading when inserting a link to a
heading that doesn't exist. This is a function which returns a list of capture templates
using the same syntax as `org-capture-templates'. The function accepts a single string
argument, which is the missing heading."
  :type 'function
  :group 'org-z)

(defun org-z-helm-org-rifle--store-link (candidate)
  "Store link to CANDIDATE."
  (-let (((buffer . pos) candidate))
    (save-excursion
      (with-current-buffer buffer
        (goto-char pos)
        (call-interactively 'org-store-link)))))

(defun org-z-helm-org-rifle--insert-link (candidate)
  "Insert link to CANDIDATE in current location."
  (interactive)
  (org-z-helm-org-rifle--store-link candidate)
  (call-interactively 'org-insert-link))

(defun org-z-capture--before-finalize-hook ()
  "During org-z-capture, store a link to the just-inserted headline."
  (let ((buf (org-capture-get :buffer))
        (point (org-capture-get :insertion-point)))
    (with-current-buffer buf
      (save-excursion
        (save-restriction
          (widen)
          (goto-char point)
          (call-interactively 'org-store-link)))))
  (remove-hook 'org-capture-before-finalize-hook #'org-z-capture--before-finalize-hook))

(defun org-z-capture--after-finalize-hook ()
  "After org-z-capture, insert a link."
  (call-interactively 'org-insert-link)
  (remove-hook 'org-capture-after-finalize-hook #'org-z-capture--after-finalize-hook))

(defun org-z-insert-missing (heading)
  "When inserting a link to a headline that doesn't exist, use a custom capture template to add the new headline."
  (let ((org-capture-templates (funcall org-z-capture-templates heading))
        goto key)
    (when (= (length org-capture-templates) 1)
      (setq keys (caar org-capture-templates)))
    (org-capture goto keys)))

(defun org-z--insert-link-to-new-heading (candidate)
  (add-hook 'org-capture-before-finalize-hook #'org-z-capture--before-finalize-hook)
  (add-hook 'org-capture-after-finalize-hook #'org-z-capture--after-finalize-hook)
  (org-z-insert-missing candidate))

(defvar org-z-insert-link--fallback
  (helm-build-dummy-source "Create link to new heading"
    :action (helm-make-actions
             "Insert link to new heading"
             #'org-z--insert-link-to-new-heading)))

;; These functions are basically just extracted from helm-org-rifle
(defun org-z--list-org-files (directories &optional toggle-recursion)
  (let* ((recursive (if (or toggle-recursion current-prefix-arg)
                        (not helm-org-rifle-directories-recursive)
                      helm-org-rifle-directories-recursive))
         (files (-flatten (--map (f-files it
                                          (lambda (file)
                                            (s-matches? helm-org-rifle-directories-filename-regexp (f-filename file)))
                                          recursive)
                                 directories))))
    (if files
        files
      (error "No org files found in directories: %s" (s-join " " directories)))))
(defun org-z--org-rifle-files-source (files)
  (--map (helm-org-rifle-get-source-for-file it) files))
(defun org-z--org-rifle-cleanup-hook ()
  (lambda ()
    ;; Close new buffers if enabled
    (when helm-org-rifle-close-unopened-file-buffers
      (if (= 0 helm-exit-status)
          ;; Candidate selected; close other new buffers
          (let ((candidate-source (helm-attr 'name (helm-get-current-source))))
            (dolist (source helm-sources)
              (unless (or (equal (helm-attr 'name source)
                                 candidate-source)
                          (not (helm-attr 'new-buffer source)))
                (kill-buffer (helm-attr 'buffer source)))))
        ;; No candidates; close all new buffers
        (dolist (source helm-sources)
          (when (helm-attr 'new-buffer source)
            (kill-buffer (helm-attr 'buffer source))))))))

;;;###autoload
(defun org-z-insert-link ()
  "Begin inserting a link to an org headline at point. A helm interface allows interactively searching for headlines to link."
  (interactive)

  (let* ((helm-candidate-separator " ")
         (helm-cleanup-hook (org-z--org-rifle-cleanup-hook))
         (org-rifle-sources (org-z--org-rifle-files-source (org-z--list-org-files org-z-directories))))
    (add-to-list 'helm-org-rifle-actions '("Insert link" . org-z-helm-org-rifle--insert-link))
    (helm
     :input (thing-at-point 'symbol 'no-properties)
     :sources (append org-rifle-sources (list org-z-insert-link--fallback))
     :buffer "*org-z-insert-link*")
    (pop helm-org-rifle-actions)))

(defun org-z-knowledge--search (targets &optional rg-opts)
  (let ((helm-rg-default-extra-args rg-opts)
        (helm-rg-default-directory (expand-file-name "~/")))
    (helm-rg
     nil
     nil
     targets)))

(defcustom org-z-knowledge-dirs org-z-directories
  "Directories in which to perform full-text knowledge search."
  :type 'list
  :group 'org-z)

(defcustom org-z-knowledge-filetypes (list "org" "md")
  "File-types in which to perform full-text knowledge search."
  :type 'list
  :group 'org-z)

;;;###autoload
(defun org-z-knowledge-search ()
  "Perform full-text search on matching files in `org-z-knowledge-dirs'."
  (interactive)
  (let ((rg-opts (flatten-list (mapcar
                                (lambda (ft)
                                  `("-t" ,ft))
                                org-z-knowledge-filetypes))))
    (org-z-knowledge--search org-z-knowledge-dirs rg-opts)))

;;;###autoload
(define-minor-mode org-z-mode
  "Minor mode for org-z."
  :lighter " org-z"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-.") 'org-z-insert-link)
            map)
  :group 'org-z
  :require 'org-z
  :global t
  (when (not org-id-link-to-org-use-id)
    (setq org-id-link-to-org-use-id t)))

(provide 'org-z)

;;; org-z.el ends here
