;;; denote-journal-capture.el --- Better Integration for Denote Journal and Org Capture -*- lexical-binding: nil -*-

;; Copyright (C) 2025  Samuel W. Flint <swflint@samuelwflint.com>

;; Author: Samuel W. Flint <swflint@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://git.sr.ht/~swflint/denote-extras
;; Version: 1.0.0
;; Keywords: convenience
;; Package-Requires: ((emacs "24.1") (denote "3.1.0"))

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

;; This library provides basic integration between
;; `denote-journal-extras' and `org-capture', providing a function to
;; allow a specific date to be captured to, while saving the date for
;; later editing as part of the capture process.
;;
;; It may be used as follows.  For a given capture template with a
;; `file' derived location, the function name
;; `denote-journal-capture-entry-for-date' may be used instead
;; of filename, for example:
;;
;; (setq org-capture-templates '(("a" "Appointment" entry
;;                                (file+olp denote-journal-capture-entry-for-date "Appointments")
;;                                "* %(denote-journal-capture-timestamp) %^{Subject?}")))
;;
;; Then, as shown above, in the template, the expansion of
;; `%(denote-journal-capture-template)' can be used to prompt
;; for (and reuse) the date that was selected as for capturing.
;;
;;;; Errors and Patches
;;
;; If you find an error, or have a patch to improve this package,
;; please send an email to ~swflint/emacs-utilities@lists.sr.ht.


;;; Code:

(require 'denote)
(require 'org)

(defvar denote-journal-capture-date nil
  "Most recently captured denote journal date.")

(defun denote-journal-capture-entry-for-date ()
  "Determine a file name for new entries."
  (let ((date (denote-date-prompt)))
    (setf denote-journal-capture-date date)
    (denote-journal-extras-path-to-new-or-existing-entry date)))

(defun denote-journal-capture-timestamp ()
  "Edit and output a timestamp based on `denote-journal-capture-date'."
  (let* ((denote-time-string (if (stringp denote-journal-capture-date)
                                 (substring denote-journal-capture-date 0 -3)
                               nil))
         (default-time (and denote-time-string (org-time-string-to-time denote-time-string)))
         (default-input (and denote-time-string (org-get-compact-tod denote-time-string)))
         org-end-time-was-given
         (timestamp (org-read-date t 'totime nil nil default-time default-input
                                   nil)))
    (setf denote-journal-capture-date nil)
    (format-time-string (if (and (stringp org-end-time-was-given)
                                 (string-match "\\([0-9]+\\):\\([0-9]+\\)" org-end-time-was-given))
                            (format "<%%Y-%%m-%%d %%a %%H:%%M-%02d:%02d>"
                                    (string-to-number (match-string 1 org-end-time-was-given))
                                    (string-to-number (match-string 2 org-end-time-was-given)))
                          "<%Y-%m-%d %a %H:%M>")
                        timestamp)))

(provide 'denote-journal-capture)
;;; denote-journal-capture.el ends here
