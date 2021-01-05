;;; org-z.el --- Lightweight, Org-mode flavored zettelkasten links -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020 Mark Hudnall <me@markhudnall.com>

;; Author: Mark Hudnall <me@markhudnall.com>
;; URL: https://github.com/landakram/org-z
;; Keywords: org-mode
;; Version: 0.0.2
;; Package-Requires: ((emacs "27.1") (org "9.3") (dash "2.12") (f "0.18.1") (s "1.10.0"))
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

(require 'cl-lib)
(require 'cl-generic)
(require 'org)
(require 'org-id)
(require 'org-capture)

(require 'f)
(require 's)
(require 'dash)

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

(defcustom org-z-refile-missing-heading nil
  "After inserting a link to a missing heading, the missing heading can be refiled. Valid options are:

nil: No refiling will occur.
'interactive: The heading will be refiled interactively.
'subheading: The heading will be automatically refiled to a subheading of the current heading (where the link is being inserted).
'sibling: The heading will be automatically refiled to a sibling of the current heading (where the link is being inserted)."
  :type 'symbol
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

(defcustom org-z-use-prescient nil
  "When t, use prescient.el to sort results. If you already have prescient enabled through
your completion system, you should leave this as nil."
  :type 'boolean
  :group 'org-z)

(defcustom org-z-completion-backend nil
  "The completion backend to use when searching for links to insert. Valid options are 'helm or 'selectrum.

Note that completion backends are implemented in separate packages. You should ensure that a completion backend
package such as org-z-selectrum is installed in addition to org-z."
  :type '(choice
          (const :tag "Helm" helm)
          (const :tag "Selectrum" selectrum))
  :group 'org-z)

(defun org-z--store-link-to-candidate (candidate)
  "Store link to CANDIDATE."
  (let ((buffer (marker-buffer candidate))
        (pos (marker-position candidate)))
    (save-excursion
      (with-current-buffer buffer
        (goto-char pos)
        (call-interactively 'org-store-link)))))

(defun org-z--insert-link-to-candidate (candidate)
  "Insert link to CANDIDATE in current location."
  (interactive)
  (org-z--store-link-to-candidate candidate)
  (call-interactively 'org-insert-link))

(defvar org-z-insert-missing-after-hook nil
  "A hook that is called after inserting a link to a missing heading.")

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
        goto keys)
    (when (= (length org-capture-templates) 1)
      (setq keys (caar org-capture-templates)))
    (org-capture goto keys)))

(defun org-z--refile-missing ()
  (let ((refile-loc
         (list
          (org-display-outline-path t t nil t)
          (buffer-file-name (buffer-base-buffer))
          nil
          (org-with-wide-buffer
           (org-back-to-heading t)
           (point-marker)))))

    (org-mark-ring-push)

    (org-capture-goto-last-stored)

    (cond ((eq org-z-refile-missing-heading 'interactive)
           (org-refile))

          ((eq org-z-refile-missing-heading 'subheading)
           (org-refile nil nil refile-loc))

          ((eq org-z-refile-missing-heading 'sibling)
           (org-refile nil nil refile-loc)
           (org-refile-goto-last-stored)
           (outline-promote)))

    (org-mark-ring-goto)))

(defun org-z--insert-link-to-new-heading (candidate)
  (add-hook 'org-capture-before-finalize-hook #'org-z-capture--before-finalize-hook)
  (add-hook 'org-capture-after-finalize-hook #'org-z-capture--after-finalize-hook)
  (org-z-insert-missing candidate)

  (when org-z-refile-missing-heading
    (org-z--refile-missing))

  (run-hooks 'org-z-insert-missing-after-hook))

(defun org-z--format-org-ql-heading (window-width)
  "Return string for completion backend for heading at point.
WINDOW-WIDTH should be the width of the window."
  (font-lock-ensure (point-at-bol) (point-at-eol))
  (let* ((prefix (concat (buffer-name) ":"))
         (width (- window-width (length prefix)))
         (heading (org-get-heading t))
         (path (-> (org-get-outline-path)
                   (org-format-outline-path width nil "")
                   (org-split-string "")))
         (path (concat (s-join "/" path) "/" heading)))
    (cons (concat prefix path) (point-marker))))

(cl-defstruct org-z--completion-backend
  org-z--insert-link)

(cl-defgeneric org-z--insert-link (org-z--completion-backend))

(defvar org-z-completion-backends '()
  "Completion backends for org-z. This is a plist where keys are `org-z-completion-backend' and
values are instances of an `org-z--completion-backend'.")

;;;###autoload
(defun org-z-insert-link (prefix)
  "Begin inserting a link to an org headline at point. A completion backend allows interactively searching for headlines to link."
  (interactive "P")
  (let* ((org-z-refile-missing-heading (cond ((equal prefix '(4))
                                              'sibling)
                                             ((equal prefix '(16))
                                              'subheading)
                                             (t
                                              org-z-refile-missing-heading)))
         (current-prefix-arg nil)
         (completion-backend (plist-get org-z-completion-backends org-z-completion-backend)))
    (when (not completion-backend)
      (user-error "Completion backend is nil! This means that either `org-z-completion-backend' is invalid, or you forgot to load one of the org-z completion backends, which are separate packages. If you use helm for example, make sure that org-z-helm is loaded in addntion to org-z."))

    (org-z--insert-link completion-backend)))

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
