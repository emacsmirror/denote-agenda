<!--
SPDX-FileCopyrightText: Copyright (C) 2025 Samuel W. Flint <swflint@samuelwflint.com>

SPDX-License-Identifier: GFDL-1.3-or-later
-->

# Denote Agenda [![Not Yet on MELPA](https://melpa.org/packages/denote-agenda-badge.svg)](https://melpa.org/#/denote-agenda) [![REUSE status](https://api.reuse.software/badge/git.sr.ht/~swflint/denote-agenda)](https://api.reuse.software/info/git.sr.ht/~swflint/denote-agenda)

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

## Errors and Patches

If you find an error, or have a patch to improve this package, please send an email to `~swflint/emacs-utilities@lists.sr.ht`.
