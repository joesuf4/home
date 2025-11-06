#!/usr/bin/zsh -i
[[ "$#" -gt 0 ]] && ./config.sh "$@"
WD="$(basename "$PWD")"
docker kill "gha_runner_$WD"
sleep 3
rm -rf _work/"$WD"
mkdir -p _work/"$WD"
sudo mount --onlyonce -t tmpfs -o size=12G,mode=1777,noatime,nosuid,nodev none _work/"$WD"
exec docker run -d --security-opt no-new-privileges --name "gha_runner_$WD" --rm -t -v "$PWD":/src -v "$PWD/_work/$WD":"/src/_work/$WD" schaefj/gha-runner:latest
