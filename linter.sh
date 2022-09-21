#!/bin/bash

: "${LINTER_DOCKER_IMAGE=schaefj/linter:latest}"

if [[ "$0" == "${0%.git/hooks/pre-commit}" ]]; then
  if [[ "$1" == install ]]; then
    if [[ -z "$OLDPWD" ]]; then
      # non-UNIX environment (Win 10?)
      cp -f linter.sh .git/hooks/pre-commit &&
        sed -i -Ee 's|^(\#!/bin/bash)|\1; C:/Windows/System32/bash.exe|' .git/hooks/pre-commit &&
        echo "pre-commit hook installed. Happy hacking!"
    else
      # UNIX shell environment
      ln -s -f ../../linter.sh .git/hooks/pre-commit &&
        echo "pre-commit hook installed. Happy hacking!"
    fi
    exit $?
  elif [[ "$1" == uninstall ]]; then
    rm .git/hooks/pre-commit && echo "pre-commit hook uninstalled."
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
  basename "$0" must match *.sh

examples:
  $0 run                          # searches $PWD for YAML files and lints them
  $0 run foo bar                  # searches foo and bar subdirs for linting
  $0 install                      # symlinks .git/hooks/pre-commit
  echo -e "foo.yml\nbar.yml" | $0 # overrides "find ..." via pipeline
  .git/hooks/pre-commit HEAD~2    # lint "git diff" against HEAD~2
EOF
    [[ "$1" == help ]]
    exit $?
  fi
fi

# punt to docker if appropriate

if [[ "$0" != "${0%.git/hooks/pre-commit}" ]] && command -v docker >/dev/null 2>&1; then
  exec docker run -t -v "$PWD":/src:ro --rm --entrypoint= "$LINTER_DOCKER_IMAGE" bash -c \
    ". ~/.asdf/asdf.sh; grep '[)]\$' linter.rc | awk '{print \$1}' | cut -d')' -f1 | xargs -P$(nproc) -d'\n' -i sh -c 'LINTER={} bash .git/hooks/pre-commit $@'"
fi

# load associated rcfile

: "${LINTER:=yamllint}" # the default linter
. ./linter.rc

# prep the (temporary) config file

trap 'rv=$?; rm -f "$CONFIG_TMP_FILE"; exit $rv' EXIT INT HUP TERM
CONFIG_TMP_FILE="$(mktemp)"
echo -e "$CONFIG_SRC" >"$CONFIG_TMP_FILE"

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
  # thus "git diff" pipeline inputs
  git diff --name-only "${@:---cached}"
fi | if [[ "${LINT_TEMPLATES:-}" == yes ]]; then cat; else grep -v /templates/; fi | grep -Pe "$PCRE_PAT" |
  while read -r line; do [[ -f "$line" ]] && echo "$line"; done |
  eval "xargs -rd'\n' -P${XARGS_WORKERS:-$(nproc)} -n${XARGS_MAX_FILES:-256} $LINTER"
