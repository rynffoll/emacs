# Checkup: find faults in the config

Compare `config.org` and `user-lisp/` with the installed Emacs and
packages.  Find:

- what is broken or obsolete;
- what you can make simpler;
- new features that are useful.

Run the commands from the repository root.  Paths that start with
`scripts/` are in the skill directory.  The scripts operate in batch
mode.  You do not need the live session.

If the user identifies one change (a commit, a NEWS section, or a
package), do steps 2, 4 and 5 only for that change.  In step 2, make
sure that the running build contains the change.

## Scope

- Read only these files: the repository, `elpa/`, and
  `/Applications/Emacs.app/Contents/Resources/` (core Lisp,
  `etc/*NEWS`).  Do not read other files in `~`.  Read upstream
  repositories with WebFetch or `curl`.
- Edit `config.org`.  Do not edit `init.el`.  Edit only after the user
  agrees with the report.
- Do not commit.

## 1. Find what changed

```sh
scripts/package-versions.el > .cache/emacs-doctor.new.tsv
scripts/package-changes.el .cache/emacs-doctor.tsv
```

`package-changes.el` prints the list of work: Emacs, if its commit
changed, and each changed package that the config needs (also
dependencies).  For a package, it gives the old and new versions, the
copies between them, the old and new `elpa/` directories (for
`diff -r`), and the NEWS/CHANGELOG files of the new copy.  The copies
are in `package.el` version order; do not sort versions in the shell.
If it prints nothing, skip steps 2 and 4.

If there is no snapshot, get the previous version of most packages from
the newest old copy in `elpa/`.  For other packages, ask the user.

## 2. Find what to read

Read only the range between the old version and the new version.

- **Emacs**: Read the headings of the current section of `etc/NEWS`.
  Then read only the entries for the packages and the functions that
  the config uses.
  If the repository commit changed, read the commit titles between the
  two commits on the upstream mirror.  `emacs-repository-version` gives
  the commit of the running build.  This command does not need `gh`
  authentication:

  ```sh
  curl -s https://api.github.com/repos/emacs-mirror/emacs/compare/A...B |
    jq -r '.behind_by, (.commits[].commit.message | split("\n")[0])'
  ```

  If `behind_by` is 0, B contains A.
- **Built-in packages with a NEWS file**: `etc/EGLOT-NEWS`,
  `etc/ORG-NEWS`, `etc/ERC-NEWS`, ...  If a built-in package is also in
  `elpa/`, read the NEWS of the copy that `locate-library` returns.
- **Packages**: Use the first source that exists:
  1. `NEWS`, `CHANGELOG*`, `CHANGES*` in the `elpa/` directory of the
     package.
  2. For a `:vc` package (a git clone):
     `git -C elpa/<pkg> log --oneline OLD..NEW`.
  3. The same files in the upstream repository.  Then its commits back
     to the date of the old version.

If many packages changed, give one subagent to each large package.  Give
one subagent to a group of small packages.  Give the subagent the
packages, the old and the new versions, the sources to read, and the
`config.org` blocks that use the packages.  The subagent returns
findings.  It does not edit.

## 3. Run the lint scripts

Run the two scripts in parallel:

```sh
timeout 300 scripts/config-lint.el
scripts/package-gc.el
```

`config-lint.el` finds:

- obsolete names;
- functions that a package removed (also private `--` functions);
- incorrect numbers of arguments;
- options that the config sets to their standard value.  You can remove
  these lines.

Emacs calculates some default values at startup (for example,
`user-full-name`).  `config-lint.el` shows them, but their value can be
different on a different computer.  Keep these lines.

`package-gc.el` shows:

- package copies that are older than the active copy and the newest old
  copy.  Keep that old copy, because the user can go back to it.
- ELPA copies that are older than their built-in package.
- unused packages: packages that no `use-package` form in `init.el`
  ensures, that no `Package-Requires` in `user-lisp/` names, and that
  no needed package requires.

The config sets `package-retention-policy` to t.  Thus, upgrades keep
all old copies.  Report the copies and the unused packages.  The user
decides about removal.  If the user agrees, delete them with
`scripts/package-gc.el --delete`.  It deletes all copies of an unused
package.  The script sets
`package-quickstart` to t.  If not, `package-delete` deletes
`package-quickstart.el`.

## 4. Compare with the config

For each change, look for:

- names that upstream removed, renamed, or made obsolete, which the
  config still uses;
- new features that can replace our `+` functions, advice, or
  workarounds;
- advice or `cl-letf` on internals that upstream changed;
- blocks that `emacs-major-version` controls and that the new Emacs
  makes unused;
- packages with a dynamic module (ghostel, macos-notifications, ...);
- new default key bindings that conflict with ours (also
  evil-collection);
- changed defaults that change behavior that we use.

For advice, make sure that the advised function still calls the
function that we patch.  If not, the advice does nothing and gives no
error.  A reinstall can remove a compiled module.  Compile it again with
the `*-compile` command of the package.

## 5. Report

Put the items in three groups.  For each item, give the package, the
version, the source line or link, and a clickable `file:LINE` for each
file (`config.org`, `user-lisp/`, `elpa/<pkg>-<ver>/`, core):

- **Broken / obsolete**: you must change it.
- **Can simplify**: you can remove or decrease our code.
- **Information**: new features that are useful.  No change is
  necessary.

Stop and wait for the user.  Then:

1. Apply the items that the user accepts to `config.org`.
2. Run `make check` one time.
3. If one form is sufficient, apply the change in the live session with
   `emacsclient -e`.
4. Tell the user what you did not check (also TTY).

When the user agrees with the result, save the snapshot:

```sh
mv .cache/emacs-doctor.new.tsv .cache/emacs-doctor.tsv
```
