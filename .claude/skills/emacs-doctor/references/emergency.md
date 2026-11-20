# Emergency: recover a hung Emacs

In most cases, a hung Emacs has one main thread that is in a Lisp loop.
The goal is to go back to the command loop and keep the buffers.  Stop
at the first step that releases Emacs.

The shell does not keep variables between commands.  Write the PID
number in each command.  Do not use `$PID`.  Paths that start with
`scripts/` are in the skill directory.

## 1. Find the function that loops

```sh
emacsclient -w 3 -e t
```

If Emacs answers, it does not hang.  It is only slow.  Repair the state
directly with `emacsclient -e` and stop here.

If the message is "can't find socket", the server is not available.  You
cannot use emacsclient.

Then find the function that loops:

```sh
pgrep -ax Emacs   # -a: also the parents. If there are many, use the Emacs of the user.
sample <PID> 1 -mayDie | sed -n '/Call graph/,/Total number in stack/p' |
  grep -E 'Thread_|\.eln\)|exec_byte_code|redisplay_internal|update_tab_bar|display_mode_line|timer_check|Fcommand_execute|garbage_collect' |
  head -40
```

Read the deepest frames of `com.apple.main-thread`.  Native-compiled
Lisp shows as `F<hex>_<lisp-function>_0 (in <file>.eln)`.  Byte-compiled
Lisp shows only as `exec_byte_code`.  Find two things:

- **The function that loops**, and where it uses its time.  If most of
  the time is in `garbage_collect`, something allocates memory without a
  limit.
- **The caller.**  `redisplay_internal` means that the loop is in
  redisplay (mode line, tab bar, header line, font-lock).  `timer_check`
  means a timer.  `Fcommand_execute` means a command or a hook.

## 2. Find what to change

In most cases, the next redisplay or command starts the loop again,
because state controls the loop.  Examples of state: a variable with a
bad value, a function on a hook or a timer, a mode-line or tab-bar
construct, an advice.

Read the source of the function that loops.  Find the guard that makes
it return early (a `(when var ...)`, a mode variable, a hook
membership).  Core sources are in
`/Applications/Emacs.app/Contents/Resources/lisp/`.

Ask the user what they changed last.  Examine `git diff --stat`, then
`git diff -- config.org user-lisp/`.

## 3. Make the state safe

If state does not control the loop, skip this step.  If state controls
the loop, do this step before the signal.  In a redisplay loop, the
redraw of the debugger starts the loop again.

```sh
scripts/lldb-set.sh <PID> VAR nil
```

Some repairs need more than nil or t (for example, to remove a hook
function or to define a function again).  These repairs need the command
loop.  Thus, set the guard to nil or t now.  Then complete the repair
with `emacsclient -e` after step 4.

## 4. Stop the current iteration

The variable alone does not stop the iteration, because the function
did its check before.

```sh
kill -USR2 <PID>
top -l 2 -pid <PID> -stats cpu | tail -1
```

If the CPU is near 0%, the loop stopped.

The default value of `debug-on-event` is `sigusr2`.  This signal stops
a loop in redisplay, but `C-g` does not.  Emacs then shows
`*Backtrace*`, and `debug-on-quit` is t.

If the signal does not stop the loop, do the same in the process:

```sh
scripts/lldb-set.sh <PID> inhibit-quit nil quit-flag t
```

## 5. Kill Emacs: only with explicit consent of the user

A general "fix it" or "go ahead" from the user is not consent for this
step.  If you kill Emacs, the user loses work that no other step
touches.  Thus:

1. Give the command.
2. Tell the user what they will lose.
3. Wait until the user confirms this specific action, or until the user
   runs it.

```sh
kill -TERM <PID>
```

SIGTERM runs `kill-emacs-hook` (savehist, recentf, desktop).  A force
quit does not.  In both cases, the user loses buffers that are not saved
and buffers without a file.

## Give blocked commands to the user

The permission layer often blocks signals and `lldb -p` on a live
process.  Do not try to bypass the block.  Prepare the exact command.
Write one line that tells what it does.  The user runs it with the `!`
prefix.

## After recovery

When Emacs is idle again:

1. If `*Backtrace*` is open, type `q` and do
   `M-x toggle-debug-on-quit`.
2. If the socket was not available, do `M-x server-start`.
3. Make sure that `emacsclient -w 3 -e t` answers.
4. Put the safe value in `config.org`.  Add a comment that names the
   failure.
5. If another Claude session set the state, tell it what occurred.
   Then it does not do the experiment again.

Example: `tab_bar_auto_width` under `redisplay_internal`, with most of
the time in `garbage_collect`.  Another agent had set
`tab-bar-auto-width` to `t`.  Do step 3 with `tab-bar-auto-width nil`,
then step 4.
