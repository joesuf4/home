setopt prompt_subst extendedglob unset interactivecomments
zmodload zsh/pcre

# enable zplug and configure fun modules

[[ -f ~/.zplug/init.zsh ]] || (
  curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh &&
    sleep 1
) && . ~/.zplug/init.zsh

zplug "plugins/ubuntu", from:oh-my-zsh
zplug "zsh-users/zsh-syntax-highlighting", defer:2
# I don't care for this menu-driven bash-like autocomplete plugin; but if you do, put it in ~/.myzshrc!
# zplug "marlonrichert/zsh-autocomplete"
zplug "zsh-users/zsh-history-substring-search"
zplug "joesuf4/zsh-history-filter"
zplug "joesuf4/zsh-autosuggestions"
#zplug "dracula/zsh", as:theme
zplug "zplug/zplug", hook-build:"zplug --self-manage"
zplug "docker/cli", use:contrib/completion/zsh

# history settings

setopt share_history extended_history hist_expire_dups_first hist_no_store

# ctrl-(up/down/right/left) arrow bindings

bindkey '\e[1;5A' history-substring-search-up
bindkey '\e[1;5B' history-substring-search-down
bindkey '\e[1;5C' emacs-forward-word
bindkey '\e[1;5D' emacs-backward-word

bindkey '^P' history-incremental-search-backward
bindkey '^N' history-incremental-search-forward

bindkey '^A' vi-beginning-of-line
bindkey '^K' kill-line
bindkey '^Y' yank
bindkey '^U' kill-region

# directory stuff

nd() {
  eval "$1='${2-$PWD}'"
  : ~$1
}

setopt autocd autopushd pushdminus pushdsilent pushdtohome
alias dh='dirs -v'

nd winhome /mnt/c/Users/$USER
nd winsrc ~winhome/src
nd winssh ~winhome/.ssh
nd r ~/src/rsim
nd c ~/src/ceval

# utilities


alias reset='reset; echoon'

alias wingit_pull='(~winhome && git pull)'

alias winemac_cp='cp ~winhome/.emacs ~/.emacs && emac ~/.emacs'

alias ptyd='pty -d pty-driver.pl --'

alias pptyd='pty -- pty -d pty-driver.pl -- 2>&1'

alias ptyon='touch /tmp/ptyon-$USER/$(basename "$(ttyname 2)");'

alias ptyoff='rm -f /tmp/ptyon-$USER/$(basename "$(ttyname 2)");'

alias :p='pon;: p'
alias :P='pon;: P'
alias :q='pon;: q'
alias :Q='pon;: Q'

pon() { ptyon; sleep 1; setopt unset }
poff() { ptyoff; sleep 1 }

oci() {
  sudo sed -i -e s/fipsmodule.cnf/fipsmodule.cnf-ootw/ /usr/local/ssl/openssl.cnf >/dev/null 2>&1
  (
    sleep 5
    sudo sed -i -e s/-ootw//g /usr/local/ssl/openssl.cnf >/dev/null 2>&1 &
  ) &
  command oci $@
}

b () { bash -ci "b $@" }

rsim_attack () {
  local rsim_dir=~r/build/bin
  for d in "${@-.}"; do
    [[ -f "$d/Parameter.csv" ]] && echo "$d";
  done |
    xargs -P$(($(nproc)/6)) -i zsh -c \
      "cd {} && $rsim_dir/rsim.exe $rsim_dir/ClearPrice.cmd /batch || true"
}

# translate between big-endian and little-endian objdumps.

alias rev_hex32='perl -ple "s/([a-f\\d]{8})/join q(), reverse \$1 =~ m!..!g/ige"'

alias dsign='DOCKER_CONTENT_TRUST=1 docker trust sign --local'

alias strip_cr="sed -i -e 's/\\r//'"

alias git_diff_branch='git diff $(git show-branch --merge-base 2>/dev/null)~1'

alias ldif_decode_base64='command perl -MMIME::Base64 -ple '\''/^([\w.-]+):: (.*)/ and $_=qq($1: ) . decode_base64($2)'\'

alias htop='_bcs_title htop; sudo -Es htop'

alias lsof='_bcs_title lsof; sudo -Es lsof'

alias bpftrace='_bcs_title bpftrace; sudo -Es bpftrace'

alias screen='command screen -U'

alias strace='sudo -E strace'

alias asdfu='asdf plugin update --all'

alias zplugu='setopt unset && zplug update; unsetopt unset'

alias npmu='npm update --location=global'

alias pip3u='pip3 freeze | cut -d= -f1 | sudo -Es xargs pip3 install -U --ignore-installed --break-system-packages'

alias gpgr='gpg --refresh-keys'

alias sps='command screen -U pty -d pty-driver.pl -- $SHELL'

alias make='TERM=xterm-256color command make -kj$(nproc)'

alias k=~/.asdf/shims/kubectl

alias tf=terraform

alias perl='command perl -CSD -Mutf8 -e "BEGIN{sub log_2 (\$) {log(shift)/log(2)}}"'

alias plint='command perl -MO=Lint'

alias log_2='perl -le "print int log_2 \$_ for @ARGV"'

alias sqrt='perl -le "print int sqrt \$_ for @ARGV"'

alias sdexec='sudo -E nsenter -t $(pidof systemd | awk "{print \$1}") -p -m -r -C'

alias gerrit_push='git push origin HEAD:refs/for/$(git branch --show-current)'

flameg() {
  local TMP="$(mktemp ~winhome/tmp/flameg-XXXX.svg)"
  local URL="file:///C:/${TMP#/mnt/c/}"
  local timing=""
  if [[ "$1" == "-t" || "$1" == "++" ]]; then
    timing="$1"
    shift
  fi

  pptyd "$@" | stackcollapse-bpftrace.pl $timing "$@" | flamegraph.pl >"$TMP"
  "$MOZILLA" "$URL"
}

# typescript file walker

tplay() {
  perl -MPOSIX=ctermid -MTerm::ReadKey -e '
    open my $t, "+<", ctermid;
    my $opt_s = grep /^-\w*s/, @ARGV;
    my $opt_c = grep /^-\w*c/, @ARGV;
    while (scalar @ARGV) {
      shift @ARGV and next if $ARGV[0] =~/^-/;
      last;
    }
    ReadMode raw => $t;
    $SIG{INT} = sub { ReadMode restore => $t };
    while ($opt_s or $opt_c or ($_=ReadKey(0,$t)) ne "q") {
      if ($opt_s or $_ eq "s" or $opt_c or $_ eq "c") {
        while (<>) {
          s/\e\[\d+;?\d{0,2}[A-Zn]//g;
          tr/\r//d;
          print if /\bScript /;
          print and last if (($opt_s or $_ eq "s") and /[#\$] /)
            or (($opt_c or $_ eq "c") and /\bcommand: /)
        }
      }
      else { s/\e\[\d+;?\d{0,2}[A-Zn]//g, tr/\r//d, print for scalar <> }
      last if eof()
    }
    ReadMode restore => $t;
  ' -- "$@"
}

# (prompt) color vars

autoload colors
colors

for COLOR in RED GREEN YELLOW WHITE BLACK CYAN BLUE MAGENTA; do
  eval PR_$COLOR='%{$fg[${(L)COLOR}]%}'
  eval PR_BRIGHT_$COLOR='%{$fg_bold[${(L)COLOR}]%}'
done
PR_RESET="%{${reset_color}%}"

# translate deep blue (which PowerShell obfuscates by default) to cyan
eval "$(dircolors <(dircolors -p | sed -e 's/DIR 01;34/DIR 00;36/'))"

# window/screen title hooks
_sec="$(date +%s)"
_disk="$(tmux-free-c-drive.sh)"
_histlines="$(wc -l ~/.zsh_history)"

precmd() {
  _delta_sec="$(($(date +%s)-_sec))"
  if [[ $_delta_sec -gt 3600 ]]; then
    _delta_sec="$((_delta_sec/3600))h$((_delta_sec/60%60))m$((_delta_sec%60))s"
  elif [[ $_delta_sec -gt 60 ]]; then
    _delta_sec="$((_delta_sec/60))m$((_delta_sec%60))s"
  else
    _delta_sec="$((_delta_sec))s"
  fi

  setopt monitor
  ptyoff
  _bcs_title

  local warn="$(tmux-free-c-drive.sh)"
  local TB=1024
  [[ "${warn%%TB}" == "$warn" ]] && TB=1
  local delta_disk=" $(awk "{d=(${warn//[^0-9.]/} - ${_disk//[^0-9.]/})*$TB; print d \"GB\"}" </dev/null)"
  [[ "$delta_disk" == " 0GB" ]] && delta_disk=""
  [[ "$warn" =~ ' [0-9]([.][0-9])?GB' ]] && warn="${PR_BRIGHT_RED}$warn"
  [[ "$warn" =~ ' [1-4][0-9]([.][0-9])?GB' ]] && warn="${PR_BRIGHT_MAGENTA}$warn"
  [[ "$warn" =~ '^[5-9][0-9]GB' ]] && warn="${PR_BRIGHT_YELLOW}$warn"

  if [[ -z "$(git ls-files --other --exclude-standard 2>/dev/null)" ]]; then
    zstyle ':vcs_info:*' formats "${PR_BLUE}${warn//☠/}💾${PR_BRIGHT_BLACK}%t ${PR_CYAN}%b${PR_BRIGHT_YELLOW}%u${PR_BRIGHT_GREEN}%c${PR_RESET}"
  else
    zstyle ':vcs_info:*' formats "${PR_BLUE}${warn//☠/}💾${PR_BRIGHT_BLACK}%t ${PR_CYAN}%b${PR_BRIGHT_YELLOW}%u${PR_BRIGHT_GREEN}%c${PR_BRIGHT_RED}✗${PR_RESET}"
  fi

  vcs_info 2>/dev/null
  unsetopt unset
  _sec="$(date +%s)"

  [[ -n "${_toggle-}" ]] && (ARGS="$_args" LINE=$_histlines DATA="$_delta_sec$delta_disk" perl -i -ple \
                              '$. == $ENV{LINE} and s{;\Q$ENV{ARGS}\E(?: # \d[^#]*(?= |$))*((?: # [^\d][^#]*(?= |$))*)(?: # [^#]*(?= |$))*}{;$ENV{ARGS}$1 # $ENV{DATA}} and print STDERR $_' \
   ~/.zsh_history 2>>(grep -qF "$_args # top_10 " && pkill -HUP tmux && tmux display-popup -T '# top_10' -w 150 -h 13 -E "tail -1000 ~/.zsh_history | grep -F '${_args//\'/'\''} #' | HIST_BLOCK=\"${HIST_BLOCK-}\" ~/bin/top_10.sh; cat -") &)
  _toggle=""
}

preexec() {
  _bcs_title $2
  _args="$2"
  _disk="$(tmux-free-c-drive.sh)"
  _histlines="$(wc -l ~/.zsh_history)"
  _sec="$(date +%s)"
  _toggle=1
}

# VCS status RPROMPT

autoload -Uz vcs_info

zstyle ':vcs_info:*' stagedstr '✔'
zstyle ':vcs_info:*' unstagedstr '➕'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{11}%r'
zstyle ':vcs_info:*' enable svn git

RPROMPT='$vcs_info_msg_0_'

# prompts (and basic utils)

if [[ ${EMACS+} == t ]]; then
  unsetopt zle
  PROMPT=$'%~%(?..(%?%))%# '
  unset RPROMPT
else
  case "$(uname)" in
    Linux)
      alias ls='command ls --color=auto'
      alias grep='command grep --color=auto'
      PROMPT=$'$PR_BRIGHT_BLACK${_delta_sec} $PR_CYAN%~$PR_BRIGHT_BLACK%(?..($PR_RED%?$PR_BRIGHT_BLACK%))$PR_BRIGHT_BLACK%#$PR_RESET '
      ;;
    FreeBSD | Darwin)
      alias ls='ls -G'
      alias grep='grep --color=auto'
      PROMPT=$'$PR_CYAN%~$PR_MAGENTA(?..($PR_RED%?$PR_MAGENTA%))$PR_MAGENTA%#$PR_RESET '
      ;;
    SunOS)
      alias ls='ls --color'
      alias grep='grep --color=auto'
      alias perlfreq="dtrace -qZn 'sub-entry { @[strjoin(strjoin(copyinstr(arg3),\"::\"),copyinstr(arg0))] = count() } END {trunc(@, 10)}'"
      alias perlperf="dtrace -qFZn 'sub-entry { start[strjoin(strjoin(copyinstr(arg3),\"::\"),copyinstr(arg0))] = timestamp } sub-return { @[strjoin(strjoin(copyinstr(arg3),\"::\"),copyinstr(arg0))] = quantize((timestamp - start[strjoin(strjoin(copyinstr(arg3),\"::\"),copyinstr(arg0))])/1000); } END {trunc(@, 10)}'"
      alias perlop="dtrace -qZn 'sub-entry { self->fqn = strjoin(copyinstr(arg3), strjoin(\"::\", copyinstr(arg0))) } op-entry /self->fqn != \"\"/ { @[self->fqn] = count() } END { trunc(@, 3) }'"
      alias solaris_ldflags='perl -ple '\''s/-L(\S+)/-L$1 -R$1/g'\'
      PROMPT=$'$PR_YELLOW%n@%m$PR_RESET:$PR_CYAN%~$PR_RESET%(?..($PR_BRIGHT_RED%?$PR_RESET%))$PR_YELLOW%#$PR_RESET '
      ;;
  esac
fi

# wrappers to enable ptyd on credential-using apps

for cmd in "${PTYON[@]}"; do
  unfunction $cmd 2>/dev/null
  exep="$(which $cmd)"
  [[ $? -eq 0 ]] && eval "$cmd() {
    if [[ $cmd == git ]]; then
      [[ \"\${1:-}\" -pcre-match '^(clone|push|pull|fetch|remote|commit|svn)\$' ]] && ptyon
    elif [[ $cmd == ssh ]]; then
      ptyon
      [[ \"\$@\" =~ \"\$OCI_HOST_PREFIX\" ]] || (sleep 6; ptyoff echo ptyoff on \$(hostname). &)&
    elif [[ $cmd == svn ]]; then
      [[ \"\${1:-}\" -pcre-match '^(up|co|ci)' ]] && ptyon
    elif [[ $cmd == sudo ]]; then
      ptyon
      local e='\$ENV{MOZILLA}=qq//'
      _bcs_title : q \$e
      sleep 1
      _bcs_title sudo \"\$@\"
    else
      ptyon
    fi
    local rv n
    for n in {1..3}; do \"$exep\" \"\$@\"; rv=\$?; [[ \$rv -eq 0 ]] && break; [[ -f /tmp/ptyon-\$USER/\$(basename \"\$(ttyname 2)\") ]] && [[ $cmd != sudo ]] || return \$rv; sleep 1; done
    if [[ -f /tmp/ptyon-\$USER/\$(basename \"\$(ttyname 2)\") && $cmd == sudo ]]; then
      e='\$ENV{MOZILLA}=qq/'\${MOZILLA/./\\\.}/
      _bcs_title : q \$e
      sleep 1
    fi
    return \$rv
  }"
done

# report_* aliases

for t in all cluster node namespace pod; do
  for n in all percent load cpu mem; do
    eval "alias report_${t}_${n}_smag='_bcs_title \"$t-$n graphs for [\$GKE_CLUSTER/\$GKE_NAMESPACE]\"; smag -n 10 \"gke report ${t//all/.} ${n//all/.} -n 5 | top_10 -n 1 | awk \\\"{ print \\\\\\\$3 }\\\"\"'"
    eval "alias report_${t}_${n}_smag_diff='_bcs_title \"$t-$n diff graphs for [\$GKE_CLUSTER/\$GKE_NAMESPACE]\"; smag -d -n 10 \"gke report ${t//all/.} ${n//all/.} -n 5 | top_10 -n 1 | awk \\\"{ print \\\\\\\$3 }\\\"\"'"
    eval "alias report_${t}_${n}_loop_100='_bcs_title \"$t-$n reports for [\$GKE_CLUSTER/\$GKE_NAMESPACE]\"; for i in {1..100}; date && gke report \"${t//all/.}\"  \"${n//all/.}\" -n 5 && sleep 10 && clear'"
    eval "alias report_${t}_${n}_forever='while :; do bcs_assume_role && report_${t}_${n}_loop_100; done'"
  done

  n=actual
  eval "alias report_${t}_${n}_smag='_bcs_title \"$t-$n graphs for [\$GKE_CLUSTER/\$GKE_NAMESPACE]\"; smag -n 10 \"gke report ${t//all/.} ${n}-cpu -n 5 | top_10 -n 1 | awk \\\"{ print \\\\\\\$3 }\\\"\" \"gke report ${t//all/.} ${n}-mem -n 5 | top_10 -n 1 | awk \\\"{ print \\\\\\\$3 }\\\"\"'"
  eval "alias report_${t}_${n}_smag_diff='_bcs_title \"$t-$n diff graphs for [\$GKE_CLUSTER/\$GKE_NAMESPACE]\"; smag -d -n 10 \"gke report ${t//all/.} ${n}-cpu -n 5 | top_10 -n 1 | awk \\\"{ print \\\\\\\$3 }\\\"\" \"gke report ${t//all/.} ${n}-mem -n 5 | top_10 -n 1 | awk \\\"{ print \\\\\\\$3 }\\\"\"'"
  eval "alias report_${t}_${n}_loop_100='_bcs_title \"$t-$n reports for [\$GKE_CLUSTER/\$GKE_NAMESPACE]\"; for i in {1..100}; date && gke report \"${t//all/.}\"  \"${n//all/.}\" -n 5 && sleep 10 && clear'"
  eval "alias report_${t}_${n}_forever='while :; do bcs_assume_role && report_${t}_${n}_loop_100; done'"

done
unalias report_node_percent_smag report_node_percent_smag_diff

alias report_node_percent_smag='_bcs_title "node-percent cpu/mem graphs for [$GKE_CLUSTER/$GKE_NAMESPACE]"; smag -n 10 "gke report node percent-cpu -n 5 | top_10 -n 1 | awk \"{ print \\\$3 }\"" "gke report node percent-mem -n 5 | top_10 -n 1 | awk \"{ print \\\$3 }\""'
alias report_node_percent_smag_diff='_bcs_title "node-percent cpu/mem diff graphs for [$GKE_CLUSTER/$GKE_NAMESPACE]"; smag -d -n 10 "gke report node percent-cpu -n 5 | top_10 -n 1 | awk \"{ print \\\$3 }\"" "gke report node percent-mem -n 5 | top_10 -n 1 | awk \"{ print \\\$3 }\""'

for t in cluster node namespace; do
  for n in cpu mem fd; do
    [[ "$t" =~ ^n ]] && eval "alias report_${t}_${n}_static=\"_report_filter_block ${t}s -$n '\\\$a ' | top_10\""
    eval "alias report_${t}_${n}_totals=\"_report_filter_block ${t}s -$n '\\\$a ' ' ' 1 | top_10\""
  done
  [[ "$t" == node ]] &&
    eval "alias report_${t}_price_static=\"_report_filter_block ${t}s -price '\\\$a ' | top_10\"" &&
    eval "alias report_${t}_age_static=\"_report_filter_block ${t}s age '\\\$a ' | top_10\"" &&
    eval "alias report_${t}_machines_static=\"_report_filter_block ${t}s provisioned-cpu 'machines \\\$ARGV ' ' ' 1 1 | top_10\"" &&
    eval "alias report_${t}_machines_totals=\"_report_filter_block ${t}s provisioned-cpu 'machines ' ' ' 1 1 | top_10\"" &&
    eval "alias report_${t}_adduser_static=\"_report_filter_block ${t}s adduser '\\\$a \\\$ARGV ' | top_10\"" &&
    eval "alias report_${t}_addkey_static=\"_report_filter_block ${t}s addkey '\\\$a \\\$ARGV ' | top_10\"" &&
    eval "alias report_${t}_sudoers_static=\"_report_filter_block ${t}s sudoers '\\\$a \\\$ARGV ' | top_10\""
done

_report_filter_block() {
  local ctx="$1"
  local match="$2"
  local prefix="${3-}"
  local sep="${4- }"
  local totals="${5-}"
  local count="${6-}"
  local suffix="${7-}"
  perl -nale "@F and \$F[-1] =~ /^[KMGTPEpnμm]i?[Bs]\$/ and \$F[-2] .= \$F[-1] and pop @F;
             \$F[-1] = 1 if @F and length \"$count\";
             (/([\\w-]*\b\Q$match\E\b)/ and \$a=\$1) ... (/Running/ and (\$a=\"\", 1))
               and (!/Running/ or (not length \$a and redo))
                 and length and (@F > 2 and \$F[-2] =~ /^(?:\Q$(tput bold)\E[^$HIST_ANCESTRY]+?[$HIST_ANCESTRY]\Q$(tput sgr0)\E)+\$/ and splice @F, -2, 1 or 1)
                   and (length \"$totals\" ? (print \"$prefix\$F[-1]\") : print \"$prefix\$ARGV$sep@F$suffix\")" /tmp/k8s/reports/$ctx/*/* |
    sed -e "s!/tmp/k8s/reports/$ctx/!!" | sort
}

alias report_all_totals='for name in cluster node namespace; do echo "\n$name mem totals...\n" && eval report_${name}_mem_totals; echo "\n$name cpu totals...\n" && eval report_${name}_cpu_totals; [[ "$name" == cluster ]] && echo "\nmonthly cost totals...\n" && report_node_monthly_totals | awk "{print \"dollars\", \$3}" | top_10; [[ "$name" == node ]] && echo "\nnode count...\n" && report_node_machines_totals; done; :'

report_node_inventory_static() {
  local ts="$(date +%s)"
  join -j 1 \
    <(join -j 1 \
      <(join -j 1 -a 1 \
        <(join -j 1 <(_report_filter_block nodes provisioned-cpu "" :) <(_report_filter_block nodes provisioned-mem "" :)) \
        <(join -j 1 <(_report_filter_block nodes percent-cpu "" :) <(_report_filter_block nodes percent-mem "" :))) \
      <(_report_filter_block nodes age "" :)) \
    <(_report_filter_block nodes -price "" : "" "" " \$a") |
    sort -k3nr | perl -nale "BEGIN{\$,=\" \"} @F == 7 and splice @F, 3, 0, (\"${PLACEHOLDER-n/a}\") x 2; push @F, map {sprintf \"%.2f\", \$_} \$F[5]*\$F[7]*24, 30*\$F[7]*24; print @F" |
    (
      echo -e "TIMESTAMP\tAWSREGION\tAWSID\tCTRXNAME\tEKSCLUSTER\tEC2HOSTNAME\tCPU\tRAM\t%CPU\t%RAM\tAGE\tINSTANCETYPE\tPRICE\tOSTYPE\tTCO\tMONTHLY"
      perl -nale "BEGIN{\$,=\"\\t\"} splice @F, 0, 1, split m![/:]!, \$F[0]; splice @F, 1, 1, split /[.]/, \$F[1]; splice @F, 1, 0, grep chomp, qx([ -z \"${PLACEHOLDER-}\" ] && $SHELL -ic \"bcs get-account-number \$F[1]\" || echo $PLACEHOLDER); unshift @F, $ts; print @F"
    )
}

alias report_node_tco_static='report_node_inventory_static | (read -r _; perl -nale "printf \"dollars %s %.2f\\n\", \$F[4], \$F[14]") | top_10'
alias report_node_tco_totals='report_node_inventory_static | (read -r _; perl -nale "\$a+=\$F[14]; END{ print \"dollars \", \$a }") | top_10'

alias report_node_monthly_static='report_node_inventory_static | (read -r _; perl -nale "printf \"dollars %s %.2f\\n\", \$F[4], \$F[15]") | top_10'
alias report_node_monthly_totals='report_node_inventory_static | (read -r _; perl -nale "\$a+=\$F[15]; END{ printf \"%s %.2f\\n\", \"dollars\", \$a }") | top_10'

alias top_10='top_10.sh'

# presumes a running emacs-server

emac() {
  local args
  args=()
  local nw=false
  local running=false
  # check if emacsclient is already running
  pgrep -U $(id -u) emacsclient >/dev/null && running=true

  # check if the user wants TUI mode
  local arg
  for arg; do
    if [[ "$arg" == "-nw" || "$arg" == "-t" || "$arg" == "--tty" ]]; then
      nw=true
    fi
  done

  # if called without arguments - open a new gui instance
  if [[ "$#" -eq "0" || "$running" != true ]]; then
    args+=(-c) # open emacsclient in a new frame
  fi
  if [[ "$#" -gt "0" ]]; then
    # if 'emac -' open standard input (e.g. pipe)
    if [[ "$1" == "-" ]]; then
      local TMP="$(mktemp /tmp/$0-stdin-XXXX)"
      cat >$TMP
      args+=(--eval '(let ((b (generate-new-buffer "*stdin*"))) (switch-to-buffer b) (insert-file-contents "'${TMP}'") (delete-file "'${TMP}'"))')
    else
      args+=("$@")
      for arg; do
        [[ "$arg" != "${arg#-}" ]] && continue
      done
    fi
  fi

  if $nw; then
    TERM=xterm-256color emacsclient "${args[@]}"
  else
    (TERM=xterm-256color nohup emacsclient "${args[@]}" </dev/null >/dev/null 2>&1 &) >/dev/null 2>&1
  fi
}

# pull in local rc config

[[ -f ~/.myzshrc ]] && . ~/.myzshrc

# install/load zplug modules

if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -r -q; then
    echo
    zplug install
  fi
fi

zplug load >/dev/null 2>&1

# reject any evaluation of unset variables

unsetopt unset

# initialize (bash+zsh)completion (extension) engines

autoload -Uz bashcompinit
bashcompinit -i

[[ -d ~/.asdf ]] && . $(asdf where gcloud)/completion.zsh.inc

# enable job control (something's turned it off somewhere on Ubuntu-21.04)

setopt monitor 2>/dev/null

# aws/tfe/k8s/bcs/eks/ec2

complete -C aws_completer aws
complete -o nospace -C terraform terraform

for sfile in ~/lib/oci_autocomplete.sh ~/.ocirc; do
  [[ -f $sfile ]] && . $sfile
done

command -v kubectl >/dev/null 2>&1 && . <(kubectl completion $(basename "$SHELL"))

. ~/.bcsrc
. ~/.gkerc
. ~/git.rc

zstyle ':completion:*' completer _expand_alias _complete _ignored

patch_swig_pl() {
  for f in ~/src/svn-1.*/subversion/bindings/swig/perl/native/*.c; do
    perl -i -0777 -pe 's#^(\s+if\(\(items [^}]+})#/*$1*/#gms' $f
    perl -i -0777 -pe 's!^(#define SWIG_PERL_OBJECT_DECL)$!$1 pTHX_!' $f
    perl -i -0777 -pe 's!^(#define SWIG_PERL_OBJECT_CALL)$!$1 aTHX_!' $f
  done
  for f in ~/src/svn-1.*/subversion/bindings/swig/perl/libsvn_swig_perl/swigutil_pl.c; do
    svn revert $f
    perl -i -0777 -pe 's#^((?!typedef)\w+\s+[^{]+{)#$1\n    dTHX;#msg' $f
  done
}

GKE_KZONE=([identity-db-cluster]=us-west1)
