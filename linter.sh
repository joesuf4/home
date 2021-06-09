#!/bin/bash

if [[ "$0" == "${0%.git/hooks/pre-commit}" ]]; then
  if [[ "$1" == install ]]; then
    ln -s ../../$(basename "$0") .git/hooks/pre-commit &&
      echo "pre-commit hook installed. Happy hacking!"
    exit $?
  elif [[ "$1" == uninstall ]]; then
    rm .git/hooks/pre-commit && echo "pre commit hook uninstalled."
    exit $?
  elif [[ "${1:-run}" != run ]]; then
    # HELP/USAGE MESSAGE BELOW
    cat <<EOF >&2
$0 [arg]

USAGE

args:
  install     - install pre-commit hook
  run         - test the hook live in "bulk-find" mode
  uninstall   - uninstall the pre-commit hook
  help        - this message

prerequisites:
  yamllint install

examples:
  $0 run                          # searches $PWD for YAML files and lints them
  $0 run foo bar                  # searches foo and bar subdirs for linting
  $0 install                      # symlinks .git/hooks/pre-commit
  echo -e "foo.yml\nbar.yml" | $0 # overrides "find ..." via pipeline
  .git/hooks/pre-commit HEAD~2    # "git diff" against HEAD~2 for YAML files
EOF
    [[ "$1" == help ]]
    exit $?
  fi
fi

# load associated rcfile (if present)

rcfile="$(realpath "$0" | sed 's/\.sh$/.rc/')"
[[ -f "$rcfile" ]] && eval ". \"$rcfile\""

# set defaults

: "${LINTER:=yamllint -c \$CONFIG_TMP_FILE}" "${PCRE_PAT:=\\.ya?ml\$}" "${CONFIG_SRC:=$(
  cat <<EOF
---
yaml-files:
  - '*.yaml'
  - '*.yml'
  - '.yamllint'
  - '.*.yaml'
  - '.*.yml'
rules:
  colons: enable
  commas: enable
  hyphens: enable
  brackets: enable
  new-lines: enable
  indentation: enable
  key-duplicates: enable
  trailing-spaces: enable
  new-line-at-end-of-file: enable
  truthy:
    level: warning
  comments-indentation:
    level: warning
  braces: disable
  document-end: disable
  document-start: disable
  empty-lines: disable
  empty-values: disable
  key-ordering: disable
  line-length: disable
  octal-values: disable
  quoted-strings: disable
  comments: disable
EOF
)}"

# prep the (temporary) config file

trap 'rv=$?; rm -f $CONFIG_TMP_FILE; exit $rv' EXIT INT HUP TERM
CONFIG_TMP_FILE=$(mktemp)
echo -e "$CONFIG_SRC" >$CONFIG_TMP_FILE

# run the configured linter

if [[ "$0" == "${0%.git/hooks/pre-commit}" ]]; then
  if [[ -p /proc/self/fd/0 || -f /proc/self/fd/0 ]]; then
    cat
  else
    [[ $# -gt 0 ]] && shift
    find "${@:-.}" -type f
  fi
else
  # using (installed ".git/hooks/pre-commit" suffix) path,
  # thus "git diff" inputs
  git diff --name-only "${@:---cached}"
fi | grep -Pe "$PCRE_PAT" |
  eval "xargs -rd'\n' -P${XARGS_WORKERS:-$(nproc)} -n${XARGS_MAX_FILES:-64} $LINTER"
