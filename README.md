<!--
SPDX-FileCopyrightText: Copyright (C) 2025 Samuel W. Flint <swflint@samuelwflint.com>

SPDX-License-Identifier: GFDL-1.3-or-later
-->

# Denote Extras

This repository contains a handful of extra integrations/utilities for [Denote](https://protesilaos.com/emacs/denote).

## Denote Agenda

This file contains a simple integration between Denote and Org-Agenda.
It is aware of `denote-journal-extras`, and provides three configuration options.

 - `denote-agenda-static-files` A list of files which should always be included.
 - `denote-agenda-include-regexp` A regexp to determine files which should be included on the fly.
 - `denote-agenda-include-journal` Set to t if `denote-journal-extras` files should be included.
   If set, only journal entries for the current and future days will be included.

To use this package, load it, configure the above options, and run:

```elisp
(denote-agenda-insinuate)
```

## Denote Journal Capture

This library provides basic integration between `denote-journal-extras` and `org-capture`, providing a function to allow a specific date to be captured to, while saving the date for later editing as part of the capture process.

It may be used as follows.
For a given capture template with a `file` derived location, the function name `denote-journal-capture-entry-for-date` may be used instead of filename, for example:

```elisp
(setq org-capture-templates '(("a" "Appointment" entry
                               (file+olp denote-journal-capture-entry-for-date "Appointments")
                               "* %(denote-journal-capture-timestamp) %^{Subject?}")))
```

Then, as shown above, in the template, the expansion of `%(denote-journal-capture-template)` can be used to prompt for (and reuse) the date that was selected as for capturing.
