;;; org-z-helm.el --- Helm completion backend for org-z -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020 Mark Hudnall <me@markhudnall.com>

;; Author: Mark Hudnall <me@markhudnall.com>
;; URL: https://github.com/landakram/org-z
;; Keywords: org-mode
;; Version: 0.0.2
;; Package-Requires: ((emacs "27.1") (org-z "0.0.2") (org-ql "0.6-pre") (helm "3.3") (helm-org "1.0"))
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

;; Helm completion backend for org-z. This is an extra package, since org-z.el doesn't depend
;; on a specific completion backend.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'org)
(require 'org-id)
(require 'org-capture)

(require 'org-z)

(require 'org-ql)
(require 'helm-org-ql)

(defun org-z-insert-link--fallback ()
  (helm-build-dummy-source "Create link to new heading"
    :action (helm-make-actions
             "Insert link to new heading"
             #'org-z--insert-link-to-new-heading)))

(defun org-z-helm-candidate-transformer (candidates _)
  (cond ((and org-z-use-prescient (fboundp 'prescient-sort-compare))
         (sort candidates (lambda (c1 c2)
                            (prescient-sort-compare (car c1) (car c2)))))
        (org-z-use-prescient
         (user-error "org-z-use-prescient is t but prescient isn't loaded. Make sure prescient is installed and required."))
        (t
         candidates)))

(defun org-z-helm-org-ql-source (buffers-files)
  (helm-make-source "org-z" 'helm-source-sync
    :candidates (lambda ()
                  (let* ((query (org-ql--query-string-to-sexp helm-pattern))
                         (window-width (window-width (helm-window))))
                    (when query
                      (with-current-buffer (helm-buffer-get)
                        (setq helm-org-ql-buffers-files buffers-files))
                      (ignore-errors
                        ;; Ignore errors that might be caused by partially typed queries.
                        (org-ql-select buffers-files query
                          :action `(org-z--format-org-ql-heading ,window-width))))))
    :match #'identity
    :filtered-candidate-transformer #'org-z-helm-candidate-transformer
    :fuzzy-match nil
    :multimatch nil
    :nohighlight t
    :volatile t
    :keymap helm-org-ql-map
    :action helm-org-ql-actions))

(defun org-z-helm-select-action-hook ()
  (let ((selection (helm-get-selection nil t)))
    (cond
     ((and org-z-use-prescient (fboundp 'prescient-remember))
      (prescient-remember selection))
     (org-z-use-prescient
      (user-error "org-z-use-prescient is t but prescient isn't loaded. Make sure prescient is installed and required.")))))

(cl-defstruct org-z--helm-backend)

(cl-defmethod org-z--insert-link ((_ org-z--helm-backend))
  (when (not (fboundp 'helm))
    (user-error "org-z-completion-backend is 'helm but helm isn't loaded. Make sure helm is installed and required."))
  (require 'helm-org-ql)

  (let* ((helm-candidate-separator " ")
         (_ (add-to-list 'helm-org-ql-actions '("Insert link" . org-z--insert-link-to-candidate)))
         (org-sources (list (org-z-helm-org-ql-source (org-ql-search-directories-files :directories org-z-directories)))))
    (add-hook 'helm-before-action-hook #'org-z-helm-select-action-hook)
    (helm
     :input (thing-at-point 'symbol 'no-properties)
     :sources (append org-sources (list (org-z-insert-link--fallback)))
     :buffer "*org-z-insert-link*")
    (pop helm-before-action-hook)
    (pop helm-org-ql-actions)))

(setq org-z-completion-backends (plist-put org-z-completion-backends 'helm (make-org-z--helm-backend)))
(setq org-z-completion-backend 'helm)

(provide 'org-z-helm)

;;; org-z-helm.el ends here
