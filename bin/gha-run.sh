#!/usr/bin/zsh -i
[[ "$#" > 0 ]] && ./config.sh "$@"
WD="$(basename "$PWD")"
rm -rf _work/"$WD"
mkdir -p _work/"$WD"
sudo mount --onlyonce -t tmpfs -o size=512M,mode=1777 none _work/"$WD"
exec docker run -d --name "gha_runner_$WD" --rm -t -v "$PWD":/src -v "$PWD/_work/$WD":"/src/_work/$WD" schaefj/gha-runner:latest
