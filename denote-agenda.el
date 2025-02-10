;;; denote-agenda.el --- Integrate Denote and Org-Agenda  -*- lexical-binding: t; -*-

;;; Copyright (C) 2025  Samuel W. Flint  <swflint@samuelwflint.com>

;; Author: Samuel W. Flint <swflint@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://git.sr.ht/~swflint/denote-extras
;; Version: 0.0.1
;; Keywords: calendar
;; Package-Requires: ((emacs "27.1") (denote "3.1.0"))

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

;; This file contains a simple integration between Denote and
;; Org-Agenda.  It is aware of `denote-journal-extras', and provides
;; three configuration options.
;;
;; - `denote-agenda-static-files' A list of files which should always
;;   be included.
;; - `denote-agenda-include-regexp' A regexp to determine files which
;;   should be included on the fly.
;; - `denote-agenda-include-journal' Set to t if
;;   `denote-journal-extras' files should be included.  If set, only
;;   journal entries for the current and future days will be included.
;; To use this package, load it, configure the above options, and run:
;;
;;     (denote-agenda-insinuate)

;;; Code:

(require 'denote)
(require 'org)

(declare-function denote-journal-extras--keyword-regex "denote-journal-extras" ())


;;; Customization
(defgroup denote-agenda ()
  "Denote/Org Agenda integration module."
  :group 'denote
  :group 'org-agenda)

(defcustom denote-agenda-static-files nil
  "Files to always include in the variable `org-agenda-files'.

See also `denote-agenda-include-regexp' and
`denote-agenda-include-journal'."
  :group 'denote-agenda
  :type '(repeat file))

(defcustom denote-agenda-include-regexp nil
  "Regular expression to determine files to include.

Note, this should not be used to select journal files, instead,
the `denote-agenda-include-journal' variable should be set.

See also `denote-agenda-static-files'."
  :group 'denote-agenda
  :type 'regexp)

(defcustom denote-agenda-include-journal (featurep 'denote-journal-extras)
  "Whether to include files from `denote-journal-extras'.

When enabled (default is based on the load-time availability of
`denote-journal-extras'), files which match the variable
`denote-journal-extras-keyword', and are on the present day or
later, will be included.

See also `denote-agenda-static-files' and
`denote-agenda-include-regexp'."
  :group 'denote-agenda
  :type 'boolean)

(defcustom denote-agenda-advise-commands
  (append
   '((org-agenda-to-appt . :before)
     (org-agenda . :before)
     (org-agenda-redo . :before)
     (org-agenda-redo-all . :before)
     (org-todo-list . :before))
   (when (featurep 'org-timeblock)
     '((org-timeblock . :before)
       (org-timeblock-list . :before)
       (org-timeblock-redraw-buffers . :before))))
  "List of functions/commands which should recalculate agenda files.

List of cons cells: function name and advice location (`:before'
or `:after').  This is processed by `denote-agenda-insinuate'."
  :group 'denote-agenda
  :type '(repeat (cons function
                       (choice :tag "Advice Location"
                               (const :tag "Before" :before)
                               (const :tag "After" :after)))))


;;; General Implementation

(defun denote-agenda-set-agenda-files (&rest _)
  "Set the variable `org-agenda-files' using denote files.

Files always include come from `denote-agenda-static-files', and
files are dynamically selected using the following variables:

 - `denote-agenda-include-regexp', which is passed to
   `denote-directory-files'.
 - If `denote-agenda-include-journal' is non-nil, journal files
   for the current and future days are included."
  (setf org-agenda-files
        (cl-remove-if (lambda (file)
                        (not (string-match-p (rx ".org" eol) file)))
                      (append denote-agenda-static-files
                              (when denote-agenda-include-regexp
                                (denote-directory-files denote-agenda-include-regexp nil t))
                              (when denote-agenda-include-journal
                                (let ((today (pcase-let ((`(_ _ _ ,day ,month ,year _ _ _) (decode-time (current-time))))
                                               (encode-time (list 0 0 0 day month year nil -1 nil)))))
                                  (cl-remove-if
                                   (lambda (filename)
                                     (let* ((identifier (denote-retrieve-filename-identifier filename))
                                            (note-date (encode-time
                                                        (cl-substitute 0 nil
                                                                       (iso8601-parse
                                                                        (denote--id-to-date identifier))
                                                                       :count 3))))
                                       (not (or (time-equal-p today note-date)
                                                (time-less-p today note-date)))))
                                   (denote-directory-files (denote-journal-extras--keyword-regex) nil t))))))))


;;; Insinuate and Integrate

(defun denote-agenda-insinuate ()
  "Insinuate `denote-agenda' into various `org-agenda' commands.

This will add advice to the functions listed in
`denote-agenda-advise-commands'."
  (dolist (advise-command denote-agenda-advise-commands)
    (pcase-let ((`(,command . ,location) advise-command))
      (advice-add command location #'denote-agenda-set-agenda-files))))

(provide 'denote-agenda)
;;; denote-agenda.el ends here
