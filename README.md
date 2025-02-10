<!--
SPDX-FileCopyrightText: Copyright (C) 2025 Samuel W. Flint <swflint@samuelwflint.com>

SPDX-License-Identifier: GFDL-1.3-or-later
-->

# Denote Extras

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
