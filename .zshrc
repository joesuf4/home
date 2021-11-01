setopt prompt_subst extendedglob unset
zmodload zsh/pcre

# enable zplug and load fun modules

[[ -f ~/.zplug/init.zsh ]] || (
  /usr/bin/curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh &&
    sleep 1
) && . ~/.zplug/init.zsh

zplug "plugins/ubuntu", from:oh-my-zsh
zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "marlonrichert/zsh-autocomplete"
zplug "zsh-users/zsh-history-substring-search"
zplug "joesuf4/zsh-history-filter"
zplug "joesuf4/zsh-autosuggestions"
#zplug "dracula/zsh", as:theme
zplug "zplug/zplug", hook-build:"zplug --self-manage"

if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -r -q; then
    echo
    zplug install
  fi
fi

zplug load >/dev/null 2>&1

# history settings

setopt share_history extended_history hist_expire_dups_first hist_no_store

# ctrl-(up/down/right/left) arrow bindings

bindkey '\e[1;5A' history-search-backward
bindkey '\e[1;5B' history-search-forward
bindkey '\e[1;5C' emacs-forward-word
bindkey '\e[1;5D' emacs-backward-word

bindkey '^P' history-substring-search-up
bindkey '^N' history-substring-search-down

# directory stuff

nd() {
  eval "$1='${2-$PWD}'"
  : ~$1
}

setopt autocd autopushd pushdminus pushdsilent pushdtohome
alias dh='dirs -v'

nd winhome /mnt/c/Users/$USER
nd winsrc ~winhome/src

# utilities

alias reset='reset; echoon'

alias wingit_pull='(~winhome && git pull)'

alias winemac_cp='cp ~winhome/.emacs ~/.emacs && emac ~/.emacs'

alias ptyd='pty -d pty-driver.pl --'

alias ptyon='touch /tmp/ptyon-$USER/$(basename "$(ttyname 2)");'

alias ptyoff='rm -f /tmp/ptyon-$USER/$(basename "$(ttyname 2)");'

# translate between big-endian and little-endian objdumps.

alias rev_hex32='perl -ple "s/([a-f\\d]{8})/join q(), reverse \$1 =~ m!..!g/ige"'

alias git_diff_branch='git diff $(git show-branch --merge-base HEAD 2>/dev/null)~1'

alias ldif_decode_base64='perl -MMIME::Base64 -ple '\''/^([\w.-]+):: (.*)/ and $_=qq($1: ) . decode_base64($2)'\'

alias htop='sudo true && ptyoff /usr/bin/sudo -E /usr/bin/htop'

alias lsof='sudo true && ptyoff /usr/bin/sudo -Es /usr/bin/lsof'

alias bpftrace='sudo true && ptyoff /usr/bin/sudo -Es bpftrace'

alias curl='/usr/bin/curl'

alias screen='TERM=xterm-256color /usr/bin/screen -U'

alias asdf='PATH="/usr/bin:$PATH" asdf'

alias strace='sudo -s /usr/bin/strace'

alias asdfu='asdf update && asdf plugin-update --all'

alias zplugu='setopt unset && zplug update; unsetopt unset'

alias npmu='sudo true && ptyoff /usr/bin/sudo -Es npm update -g'

alias pip3u='pip3 freeze | cut -d= -f1 | sudo xargs pip3 install -U'

alias sps='screen pty -d pty-driver.pl -- $SHELL'

alias make='TERM=xterm-256color make -kj$(nproc)'

alias k=kubectl

alias perl='perl -CSD -Mutf8 -e "BEGIN{sub log_2 (\$) {log(shift)/log(2)}}"'

alias plint='command perl -MO=Lint'

alias log_2='perl -le "print int log_2 \$_ for @ARGV"'

alias sqrt='perl -le "print int sqrt \$_ for @ARGV"'

alias sbei='seed_bastion_ec2_inventory ~/src/*-deployer'

alias sdexec='sudo nsenter -t $(pidof systemd) -a'

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
eval $(dircolors -p | sed -e 's/DIR 01;34/DIR 00;36/' | dircolors /dev/stdin)

# window/screen title hooks

precmd() {
  ptyoff
  _bcs_title

  if [[ -z "$(git ls-files --other --exclude-standard 2>/dev/null)" ]]; then
    zstyle ':vcs_info:*' formats "${PR_BRIGHT_BLACK}[${PR_RESET}${PR_CYAN}%b${PR_BRIGHT_YELLOW}%u${PR_BRIGHT_GREEN}%c${PR_BRIGHT_BLACK}]${PR_RESET}"
  else
    zstyle ':vcs_info:*' formats "${PR_BRIGHT_BLACK}[${PR_RESET}${PR_CYAN}%b${PR_BRIGHT_YELLOW}%u${PR_BRIGHT_GREEN}%c${PR_BRIGHT_RED}✗${PR_BRIGHT_BLACK}]${PR_RESET}"
  fi

  vcs_info 2>/dev/null
}

preexec() {
  _bcs_title $2
}

# VCS status RPROMPT

autoload -Uz vcs_info

zstyle ':vcs_info:*' stagedstr '✔'
zstyle ':vcs_info:*' unstagedstr '✚'
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
  alias ls='ls --color=auto'
  alias grep='grep --color=auto'
  PROMPT=$'$PR_CYAN%~$PR_RESET$PR_BRIGHT_BLACK%(?..($PR_RESET$PR_RED%?$PR_BRIGHT_BLACK%))$PR_BRIGHT_BLACK%#$PR_RESET '
fi

# wrappers to enable ptyd on credential-using apps

for cmd in "${PTYON[@]}"; do
  unfunction $cmd 2>/dev/null
  exep="$(which $cmd)"
  [[ $? -eq 0 ]] && eval "$cmd() {
    if [[ $cmd == git ]]; then
      [[ \"\${1:-}\" -pcre-match '^(clone|push|pull|fetch|remote|commit)\$' ]] && ptyon
    elif [[ $cmd == ssh ]]; then
      local arg PEMFILE
      PEMFILE=\"\$(mktemp /tmp/bastion-ec2-ssh-id-XXXXX.pem)\"
      for arg; do [[ \"\$arg\" == \"\${arg#-}\" && \"\$EC2_ID[\$arg]\" =~ ^/ ]] && set -- -i \"\$PEMFILE\" \"\$@\" && break; done
      [[ \"\$EC2_ID[\$arg]\" =~ ^/ ]] && pty -nie -- pty -d pty-driver.pl -- \$SHELL -ic 'ansible-vault decrypt --output \"\$@\"' -- \"\$PEMFILE\" \"\$EC2_ID[\$arg]\" </dev/null >/dev/null 2>&1
      ptyoff
    else
      ptyon
    fi
    local rv n
    for n in {1..3}; do \"$exep\" \"\$@\"; rv=\$?; [[ \$rv -eq 0 ]] && break; [[ -f /tmp/ptyon-\$USER/\$(basename \"\$(ttyname 2)\") ]] && [[ $cmd != sudo ]] || return \$rv; sleep 1; done
    [[ -f /tmp/ptyon-\$USER/\$(basename \"\$(ttyname 2)\") ]] && sleep 1
    [[ $cmd == ssh ]] && rm -rf \"\$PEMFILE\"
    return \$rv
  }"
done

# report_* aliases

for t in all cluster node namespace pod; do
  for n in all percent load actual cpu mem; do
    eval "alias report_${t}_${n}_loop_100='_bcs_title \"$t-$n reports for [\$EKS_CLUSTER/\$EKS_NAMESPACE]\"; for i in {1..100}; date && eks report \"${t//all/.}\"  \"${n//all/.}\" -n 5 && sleep 10 && clear'"
    eval "alias report_${t}_${n}_forever='while :; do bcs_assume_role && report_${t}_${n}_loop_100; done'"
  done
done

for t in cluster node namespace; do
  for n in cpu mem fd; do
    [[ "$t" =~ ^n ]] && eval "alias report_${t}_${n}_static=\"_report_filter_block ${t}s -$n '\\\$a ' | top_10\""
    eval "alias report_${t}_${n}_totals=\"_report_filter_block ${t}s -$n '\\\$a ' ' ' 1 | top_10\""
  done
  [[ "$t" == node ]] &&
    eval "alias report_${t}_price_static=\"_report_filter_block ${t}s -price '\\\$a ' | top_10\"" &&
    eval "alias report_${t}_age_static=\"_report_filter_block ${t}s age '\\\$a ' | top_10\"" &&
    eval "alias report_${t}_machines_static=\"_report_filter_block ${t}s provisioned-cpu 'machines \\\$ARGV ' ' ' 1 1 | top_10\"" &&
    eval "alias report_${t}_machines_totals=\"_report_filter_block ${t}s provisioned-cpu 'machines ' ' ' 1 1 | top_10\""
done

_report_filter_block() {
  local ctx="$1"
  local match="$2"
  local prefix="${3-}"
  local sep="${4- }"
  local totals="${5-}"
  local count="${6-}"
  local suffix="${7-}"
  perl -nale "@F and \$F[-1] =~ /^[KMGTpnμm]i?[Bs]\$/ and \$F[-2] .= \$F[-1] and pop @F;
             \$F[-1] = 1 if @F and length \"$count\";
             (/([\\w-]*\b\Q$match\E\b)/ and \$a=\$1) ... (/Running/ and (\$a=\"\", 1))
               and (!/Running/ or (not length \$a and redo))
                 and length and (@F > 2 and \$F[-2] =~ /^(?:\Q$(tput bold)\E[^x☠◆▬■●▶]+?[x☠◆▬■●▶]\Q$(tput sgr0)\E)+\$/ and splice @F, -2, 1 or 1)
                   and (length \"$totals\" ? (print \"$prefix\$F[-1]\") : print \"$prefix\$ARGV$sep@F$suffix\")" /tmp/k8s/reports/$ctx/*/* |
    sed -e "s!/tmp/k8s/reports/$ctx/!!" | sort
}

alias report_all_totals='for name in cluster node namespace; do echo "\n$name mem totals...\n" && eval report_${name}_mem_totals; echo "\n$name cpu totals...\n" && eval report_${name}_cpu_totals; [[ "$name" == cluster ]] && echo "\nmonthly cost totals...\n" && report_node_monthly_totals | awk "{print \"dollars\", \$3}" | top_10; [[ "$name" == node ]] && echo "\nnode count...\n" && report_node_machines_totals; done; :'

report_node_inventory_static() {
  local ts=$(date +%s)
  join -j 1 \
    <(join -j 1 \
      <(join -j 1 -a 1 \
        <(join -j 1 <(_report_filter_block nodes provisioned-cpu "" :) <(_report_filter_block nodes provisioned-mem "" :)) \
        <(join -j 1 <(_report_filter_block nodes percent-cpu "" :) <(_report_filter_block nodes percent-mem "" :))) \
      <(_report_filter_block nodes age "" :)) \
    <(_report_filter_block nodes -price "" : "" "" " \$a") |
    sort -k3nr | perl -nale "BEGIN{\$,=\" \"} @F == 7 and splice @F, 3, 0, (\"${PLACEHOLDER-n/a}\") x 2; push @F, map {sprintf \"%.2f\", \$_} \$F[5]*\$F[7]*24, 30*\$F[7]*24; print @F" |
    (
      echo -e "TIMESTAMP\tAWSREGION\tAWSORGID\tBXORGNAME\tEKSCLUSTER\tEC2HOSTNAME\tCPU\tRAM\t%CPU\t%RAM\tAGE\tINSTANCETYPE\tPRICE\tOSTYPE\tTCO\tMONTHLY"
      perl -nale "BEGIN{\$,=\"\\t\"} splice @F, 0, 1, split m![/:]!, \$F[0]; splice @F, 1, 1, split /[.]/, \$F[1]; splice @F, 1, 0, grep chomp, qx([ -z \"${PLACEHOLDER-}\" ] && $SHELL -ic \"bcs get-account-number \$F[1]\" || echo $PLACEHOLDER); unshift @F, $ts; print @F"
    )
}

alias report_node_tco_static='report_node_inventory_static | (read -r _; perl -nale "printf \"dollars %s %.2f\\n\", \$F[4], \$F[14]") | top_10'
alias report_node_tco_totals='report_node_inventory_static | (read -r _; perl -nale "\$a+=\$F[14]; END{ print \"dollars \", \$a }") | top_10'

alias report_node_monthly_static='report_node_inventory_static | (read -r _; perl -nale "printf \"dollars %s %.2f\\n\", \$F[4], \$F[15]") | top_10'
alias report_node_monthly_totals='report_node_inventory_static | (read -r _; perl -nale "\$a+=\$F[15]; END{ printf \"%s %.2f\\n\", \"dollars\", \$a }") | top_10'

top_10() {
  # accepts:
  #   ANSI_COLOR_ID (defaults to 1=red)
  #   HIST_BLOCK (defaults to "x")
  #   COL (-umn width, defaults to 40), and
  #   KB (SI Kilo multiplier, defaults to 1024) env vars;
  # can process its own output (verbatim or otherwise)
  perl -nale "BEGIN { \$KB=${KB-1024}; \$UNIT=-4; }
              END {
                \$DIV = \$KB**(\$UNIT);
                for (sort {\$h{\$b} <=> \$h{\$a}} keys %h) {
                  \$SCALE //= do {
                       if (\$h{\$_}/\$DIV > ${COL-40}**2) {\"log_2\"}
                    elsif (\$h{\$_}/\$DIV > ${COL-40})    {\"sqrt\"}
                    else                                  {\"\"}
                  };
                  printf \"%${COL-40}s %s %s%s\\n\",
                    \$_,
                    \"$(tput bold)$(tput setaf ${ANSI_COLOR_ID-1})${HIST_BLOCK-x}$(tput sgr0)\" x
                      eval \"\$SCALE(\$h{\$_}/\$DIV)\",
                    (eval          \"\$h{\$_}/\$DIV\"),
                    (\"\", map \" \$_\".(\$KB==1024 && 'i').(\"\", \"B\", \"s\")[\$UNIT<=>0],
                             qw/K M G T p n μ m/)[\$UNIT]
                }
              }
              next unless /\\S\\s+[+-]?[\\d.]+\\w*\\b/;
              \$F[-1] =~ /^[KMGTpnμm]i?[Bs]\$/ and \$F[-2] .= \$F[-1] and pop @F;
              s/:\$// for @F;
              my \$unit = 0;
              for (\$F[-1]) {
                s/T/*(\$KB**4)/i   and \$unit = 4;
                s/G/*(\$KB**3)/i   and \$unit = 3;
                s/M/*(\$KB**2)/    and \$unit = 2;
                s/K/*\$KB/i        and \$unit = 1;
                s!m!/1000!         and \$unit =-1 and \$KB=1000;
                s![μu]!/(1000**2)! and \$unit =-2 and \$KB=1000;
                s!n!/(1000**3)!    and \$unit =-3 and \$KB=1000;
                s!p!/(1000**4)!    and \$unit =-4 and \$KB=1000;
                tr!0-9*/().+-!!dc;
                \$_ = eval
              }
              \$UNIT = \$unit if \$unit > \$UNIT;
              \$h{+join \" \", grep !/^(?:\Q$(tput bold)\E[^x☠◆▬■●▶]+?[x☠◆▬■●▶]\Q$(tput sgr0)\E)+$/, @F[0..(\$#F-1)]} += \$F[-1]" | head "$@"
}

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
        [[ -e "$arg" ]] || echo >"$arg"
      done
    fi
  fi

  if $nw; then
    emacsclient "${args[@]}"
  else
    (nohup emacsclient "${args[@]}" </dev/null >/dev/null 2>&1 &) >/dev/null 2>&1
  fi
}

seed_vault_pass() {
  local TMP="$(mktemp)"
  (
    bcs assume-role devops-nonprod engineer >/dev/null &&
      PW="$(aws secretsmanager get-secret-value --secret-id service-deployer-ansible-vault-pass |
        jq .SecretString | tr -d \")"
    (printf "%s\n%s\n" "$PW" "$PW" && sleep 1) | pty -nie -- pty -d pty-driver.pl ansible-vault encrypt "$TMP"
  )
  rm "$TMP"
}

seed_bastion_ec2_inventory() {
  EC2_ID_SRC="$(for dir in "$@"; do
    cd $dir && grep ansible_ provisioning/inventory/*/hosts | cut -d: -f2 | awk "{gsub(\"^.*/\", \"$dir/\", \$3); print \$1, \$3\".encrypted\"}" | sort -u | grep -Fv 10.161.160.
  done)"
  BCS_PROFILE=n/a _ec2_load_inventory
}

# reject any evaluation of unset variables

unsetopt unset

# initialize (bash+zsh)completion (extension) engines

autoload -Uz bashcompinit
bashcompinit -i

. ~/.asdf/completions/asdf.bash

# aws/tfe/k8s/bcs/eks/ec2

complete -C aws_completer aws
complete -o nospace -C terraform terraform

. <(kubectl completion $(basename "$SHELL"))
. ~/.bcsrc
. ~/.ec2rc
. ~/.eksrc
