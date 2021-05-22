setopt prompt_subst extendedglob

# history settings
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history

setopt share_history extended_history hist_expire_dups_first hist_no_store

# ctrl-arrow (up/down/left/right) key bindings

if [[ "$(uname)" == "Darwin" ]]; then
  bindkey '^[[A' history-search-backward
  bindkey '^[[B' history-search-forward
  bindkey '^[[C' emacs-forward-word
  bindkey '^[[D' emacs-backward-word
fi
bindkey ';5A' history-search-backward
bindkey ';5B' history-search-forward
bindkey '5A' history-search-backward
bindkey '5B' history-search-forward
bindkey ';5C' emacs-forward-word
bindkey ';5D' emacs-backward-word
bindkey '5C' emacs-forward-word
bindkey '5D' emacs-backward-word

# directory stuff

nd() { eval "$1='${2-$PWD}'"; : ~$1 }
DIRSTACKSIZE=8
setopt autocd autopushd pushdminus pushdsilent pushdtohome
alias dh='dirs -v'

nd winhome /mnt/c/Users/$USER
nd src ~winhome/src

# typescript file walker

tplay() {
  perl -MPOSIX=ctermid -MTerm::ReadKey -e '
    open my $t, "+<", ctermid;
    my $opt_s = grep /s/, @ARGV;
    my $opt_c = grep /c/, @ARGV;
    ReadMode raw => $t;
    while ($opt_s or $opt_c or ($_=ReadKey(0,$t)) ne "q") {
      if ($opt_s or $_ eq "s" or $opt_c or $_ eq "c") {
        while (<STDIN>) {
          s/\e\[\d+;?\d{0,2}[A-Zn]//g;
          tr/\r//d;
          print if /\bScript /;
          print and last if (($opt_s or $_ eq "s") and /[#\$] /)
            or (($opt_c or $_ eq "c") and /\bcommand: /)
        }
      }
      else { s/\e\[\d+;?\d{0,2}[A-Zn]//g, tr/\r//d, print for scalar <STDIN> }
      last if eof(STDIN)
    }
    ReadMode restore => $t;
  ' -- "$@"
}

# color vars

autoload colors
colors

for COLOR in RED GREEN YELLOW WHITE BLACK CYAN BLUE MAGENTA; do
    eval PR_$COLOR='%{$fg[${(L)COLOR}]%}'
    eval PR_BRIGHT_$COLOR='%{$fg_bold[${(L)COLOR}]%}'
done
PR_RESET="%{${reset_color}%}";

# translate deep blue (which PowerShell obfuscates by default) to cyan
eval $(dircolors -p | sed -e 's/DIR 01;34/DIR 00;36/' | dircolors /dev/stdin)

# window/screen title hooks

precmd() {
  _bcs_title

  if [[ -z "$(git ls-files --other --exclude-standard 2>/dev/null)" ]] {
    zstyle ':vcs_info:*' formats "[${PR_CYAN}%b${PR_BRIGHT_GREEN}%c${PR_BRIGHT_YELLOW}%u${PR_RESET}]"
  } else {
    zstyle ':vcs_info:*' formats "[${PR_CYAN}%b${PR_BRIGHT_GREEN}%c${PR_BRIGHT_YELLOW}%u${PR_BRIGHT_RED}?${PR_RESET}]"
  }

  vcs_info 2>/dev/null
}

preexec() { _bcs_title $2 }

# VCS status RPROMPT

autoload -Uz vcs_info

zstyle ':vcs_info:*' stagedstr 'S'
zstyle ':vcs_info:*' unstagedstr 'M'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{11}%r'
zstyle ':vcs_info:*' enable svn git

RPROMPT='$vcs_info_msg_0_'

# various platform colorized prompts (and basic utils)

if [[ ${EMACS+} == t ]]; then
  unsetopt zle
  PROMPT=$'%n@%m:%~%(?..(%?%))%# '
  unset RPROMPT
else
  case "$(uname)" in

    FreeBSD|Darwin)
      alias ls='ls -G'
      alias grep='grep --color=auto'
      PROMPT=$'$PR_BLACK%n@%m$PR_RESET:$PR_BLUE%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))$PR_BLACK%#$PR_RESET '
      ;;

    Linux)
      alias ls='ls --color=auto'
      alias grep='grep --color=auto'
      PROMPT=$'$PR_CYAN%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))%#$PR_RESET '
      ;;

    SunOS)
      alias ls='ls -F'
      alias grep='ggrep --color=auto'
      PROMPT=$'$PR_YELLOW%n@%m$PR_RESET:$PR_BLUE%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))$PR_YELLOW%#$PR_RESET '
      ;;

    *)
      PROMPT=$'%n@%m:%~%(?..(%?%))%# '
      ;;
  esac
fi

# utilities

# translate between big-endian and little-endian objdumps.
alias rev_hex32='perl -ple "s/([a-f\\d]{8})/join q(), reverse \$1 =~ m!..!g/ige"'

alias gerrit_push='git push origin HEAD:refs/for/$(git branch --show-current)'

alias ldif_decode_base64='perl -MMIME::Base64 -ple '\''/^([\w.-]+):: (.*)/ and $_=qq($1: ) . decode_base64($2)'\'

alias solaris_ldflags='perl -ple '\''s/-L(\S+)/-L$1 -R$1/g'\'

alias htop='sudo -Es htop'

alias lsof='sudo -Es lsof'

alias bpftrace='sudo -Es bpftrace'

alias apt='sudo -E apt'

alias top_10='perl -nale "END{ print \"\$_\\t\" . (\"x\" x ${FCN-}(\$h{\$_}/${DIV-1}) . \" \$h{\$_}\" for sort {\$h{\$b} <=> \$h{\$a}} keys %h} \$h{\$F[0]} = \$F[1]" | head'

alias wingit_pull='(~winhome && git pull)'

alias winemac_cp='cp ~winhome/.emacs ~/.emacs && emac ~/.emacs'

alias pd='pty -d pty-driver.pl --'

alias make='TERM=xterm-256color make -kj$(nproc)'

# presumes a running emacs-server

emac() {
  local args; args=()
  local nw=false
  local running=false
  # check if emacsclient is already running
  pgrep -U $(id -u) emacsclient > /dev/null && running=true

  # check if the user wants TUI mode
  local arg;
  for arg; do
    if [[ "$arg" == "-nw" || "$arg" == "-t" || "$arg" == "--tty" ]]
    then
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
    fi
  fi

  if $nw; then
    emacsclient "${args[@]}"
  else
    (nohup emacsclient "${args[@]}" </dev/null >/dev/null 2>&1 &) > /dev/null 2>&1
  fi
}

# reject any use of unset variable "evaluation"

unsetopt unset

# initialize (bash+zsh)completion (extension) engines

autoload -Uz bashcompinit
bashcompinit -i

. ~/.asdf/completions/asdf.bash

# aws/tfe/k8s/ec2/eks/ec2

complete -C aws_completer aws
complete -o nospace -C terraform terraform

. <(kubectl completion $(basename "$SHELL"))
. ~/.bcsrc
. ~/.ec2rc
. ~/.eksrc
