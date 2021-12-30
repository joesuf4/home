#!/usr/bin/zsh -i

if ! [[ -d ~/src/bcscli ]]; then
  echo "BCSCLI git repo not cloned to ~src/bcscli!" >&2
  exit 2
fi

# nodes, namespaces, or clusters
type=$1
shift

for sig in EXIT TERM INT HUP; do
  trap 'cp ~/src/bcscli/eksrc ~/.eksrc' $sig
done

(
  echo "_eks_${type%s}_report_cmds=($@)"
  cat
) >>~/.eksrc

eks report global $type
