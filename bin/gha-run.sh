#!/usr/bin/zsh
[[ "$#" > 0 ]] && ./config.sh "$@"
exec docker run -d -t -v "$PWD":/src -v --rm schaefj/gha-runner:latest
