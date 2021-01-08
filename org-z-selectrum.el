;;; org-z-selectrum.el --- Selectrum completion backend for org-z -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2020 Mark Hudnall <me@markhudnall.com>

;; Author: Mark Hudnall <me@markhudnall.com>
;; URL: https://github.com/landakram/org-z
;; Version: 0.0.3
;; Package-Requires: ((emacs "27.1") (org-z "0.0.2") (org-ql "0.6-pre") (selectrum "3.0"))
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

;; Selectrum completion backend for org-z. This is an extra package, since org-z.el doesn't depend
;; on a specific completion backend.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'org)
(require 'org-id)
(require 'org-capture)

(require 'org-z)

(require 'org-ql)
(require 'org-ql-search)
(require 'selectrum)

(cl-defstruct org-z--selectrum-backend)

(cl-defmethod org-z--insert-link ((_ org-z--selectrum-backend))
  (when (not (fboundp 'selectrum-read))
    (user-error "org-z-completion-backend is 'selectrum but selectrum isn't loaded. Make sure selectrum is installed and required."))

  (let* ((buffers-files (org-ql-search-directories-files :directories org-z-directories))
         (candidate-fn (lambda (pattern)
                         (let* ((query (org-ql--query-string-to-sexp pattern))
                                (window-width (window-text-width)))
                           (when query
                             (ignore-errors
                               ;; Ignore errors that might be caused by partially typed queries.
                               (org-ql-select buffers-files query
                                 :action `(org-z--format-org-ql-heading ,window-width)))))))
         (result (selectrum-read "Insert link: " candidate-fn
                                 :initial-input (thing-at-point 'symbol 'no-properties))))
    (if-let ((point-marker (get-text-property 0 'point-marker result)))
        (org-z--insert-link-to-candidate point-marker)
      (org-z--insert-link-to-new-heading result))))

(setq org-z-completion-backends (plist-put org-z-completion-backends 'selectrum (make-org-z--selectrum-backend)))
(setq org-z-completion-backend 'selectrum)

(provide 'org-z-selectrum)

;;; org-z-selectrum.el ends here
