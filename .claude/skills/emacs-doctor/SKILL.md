---
name: emacs-doctor
description: Find and repair faults in the running GNU Emacs of the user and in its config. Use it when Emacs hangs, uses 100% CPU, or does not redraw, or when `emacsclient` does not answer. Use it after an upgrade of Emacs or packages, and for a check of the config. Use it also when the user gives a commit, a NEWS or changelog link, or an error after an upgrade, also with no other text. Do not use it for new features or key bindings.
---

# Emacs doctor

Select the procedure from the request of the user.  Read only the
reference for that procedure:

|Symptom                                                                                        |Reference                                         |
|-----------------------------------------------------------------------------------------------|--------------------------------------------------|
|Emacs hangs, uses 100% CPU, does not redraw, or `emacsclient` times out                        |[references/emergency.md](references/emergency.md)|
|Emacs or packages are upgraded, the user gives a commit, NEWS or changelog, or asks for a check|[references/checkup.md](references/checkup.md)    |

If the request does not identify a procedure (for example, a bare
`/emacs-doctor`), ask with `AskUserQuestion`.  Do not ask when Emacs
hangs.
