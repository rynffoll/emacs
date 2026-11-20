#!/bin/sh
# Set global Emacs variables to nil or t inside a running (possibly hung)
# Emacs by calling Fset through lldb.
#
# Attaching may stop Emacs mid-GC, so the calls wait on an Ffuncall
# breakpoint first.  No debug info: calls are cast, Lisp_Object as
# unsigned long.  Qnil is 0; Qt is interned.
set -eu

usage() { echo "usage: $0 PID VAR {nil|t} [VAR {nil|t} ...]" >&2; exit 2; }

[ $# -ge 3 ] && [ $(($# % 2)) -eq 1 ] || usage
PID=$1; shift

IN='((unsigned long(*)(const char*, long))intern_1)'
FS='((unsigned long(*)(unsigned long, unsigned long))Fset)'
FV='((unsigned long(*)(unsigned long))Fsymbol_value)'
QT="${IN}(\"t\", 1)"

echo "pid $PID: $* (raw value before/after; 0 = nil)"
SCRIPT=$(mktemp -t lldb-set)
trap 'rm -f "$SCRIPT"' EXIT
{
  echo 'settings set stop-disassembly-display never'
  echo 'breakpoint set -n Ffuncall'
  echo 'continue'
  i=0
  while [ $# -gt 0 ]; do
    case $2 in nil) RHS=0 ;; t) RHS=$QT ;; *) usage ;; esac
    echo "expr -- unsigned long \$s$i = ${IN}(\"$1\", ${#1}); ${FV}(\$s$i)"
    echo "expr -- ${FS}(\$s$i, ${RHS}); ${FV}(\$s$i)"
    i=$((i + 1)); shift 2
  done
  echo 'breakpoint delete -f'
  echo 'detach'
} > "$SCRIPT"

timeout 60 lldb -p "$PID" --batch -s "$SCRIPT"
