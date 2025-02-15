<!--
SPDX-FileCopyrightText: Copyright (C) 2025 Samuel W. Flint <swflint@samuelwflint.com>

SPDX-License-Identifier: GFDL-1.3-or-later
-->

# Denote Extras [![REUSE status](https://api.reuse.software/badge/git.sr.ht/~swflint/denote-extras)](https://api.reuse.software/info/git.sr.ht/~swflint/denote-extras)

This repository contains a handful of extra integrations/utilities for [Denote](https://protesilaos.com/emacs/denote).

## Denote Agenda [![Not Yet on MELPA](https://melpa.org/packages/denote-agenda.svg)](https://melpa.org/#/denote-agenda)

This file contains a simple integration between Denote and Org-Agenda.
It is aware of `denote-journal-extras`, and provides three configuration options.

 - `denote-agenda-static-files` A list of files which should always be included.
 - `denote-agenda-include-regexp` A regexp to determine files which should be included on the fly.
 - `denote-agenda-include-journal` Set to t if `denote-journal-extras` files should be included.
   If set, only journal entries for the current and future days will be included.
- `denote-agenda-include-journal-limit` Set to nil if all current/future journal entries should be included, or a positive number specifying how many should be included.

To use this package, load it, configure the above options, and run:

```elisp
(denote-agenda-insinuate)
```

## Denote Journal Capture [![Not Yet on MELPA](https://melpa.org/packages/denote-journal-capture.svg)](https://melpa.org/#/denote-journal-capture)

This library provides basic integration between `denote-journal-extras` and `org-capture`, providing a function to allow a specific date to be captured to, while saving the date for later editing as part of the capture process.

It may be used as follows.
For a given capture template with a `file'`derived location, the functions `denote-journal-capture-entry-for-date` (prompt for a date) or `denote-journal-capture-entry-for-today` (today's entry) may be used instead of filename, for example:

```elisp
(setq org-capture-templates '(("a" "Appointment" entry
                               (file+olp denote-journal-capture-entry-for-date "Appointments")
                               "* %(denote-journal-capture-timestamp) %^{Subject?}")))
```

Then, as shown above, in the template, the expansion of `%(denote-journal-capture-template)` can be used to prompt for (and reuse) the date that was selected as for capturing.

## Errors and Patches

If you find an error, or have a patch to improve this package, please send an email to `~swflint/emacs-utilities@lists.sr.ht`.
