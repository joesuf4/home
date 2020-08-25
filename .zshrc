HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history
setopt share_history extended_history hist_expire_dups_first hist_no_store prompt_subst extendedglob
unsetopt unset

# ctrl-(up/down/left/right) bindings

if [[ "$(uname)" == "Darwin" || "$(uname)" == "SunOS" ]]; then
    bindkey '^[[A' history-search-backward
    bindkey '^[[B' history-search-forward
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

nd () { export $1=$PWD; : ~$1 }
DIRSTACKSIZE=8
setopt autocd autopushd pushdminus pushdsilent pushdtohome
alias dh='dirs -v'

# typescript file walker

tplay () {
    perl -MPOSIX=ctermid -MTerm::ReadKey -e '
        open my $t, "+<", ctermid;
        ReadMode raw => $t;
        my $opt = shift eq "-s";
        while ($opt or ($_=ReadKey(0, $t)) ne "q") {
            if ($opt or $_ eq "s") { while(<>) { print and last if /[#%\$] / }}
            else { print scalar <> }
            last if eof
        }
        ReadMode restore => $t
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

# window titles

title () {
    # screen title
    [[ "$TERM" == "screen" || "$(uname)" == "SunOS" ]] && print -Pn "\ek$1\e\\"
    # xterm title
    print -Pn "\e]0;%n@%m: "
    print -rn $1
    print -Pn " [%j]\a"
}

precmd () {
    title "zsh"

    if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
        zstyle ':vcs_info:*' formats "[${PR_BLACK}%b${PR_BRIGHT_GREEN}%c${PR_BRIGHT_YELLOW}%u${PR_RESET}]"
    } else {
        zstyle ':vcs_info:*' formats "[${PR_BLACK}%b${PR_BRIGHT_GREEN}%c${PR_BRIGHT_YELLOW}%u${PR_BRIGHT_RED}?${PR_RESET}]"
    }

    vcs_info 2>/dev/null
}

preexec () { title $2 }


# VCS status RPROMPT

autoload -Uz vcs_info

zstyle ':vcs_info:*' stagedstr 'S'
zstyle ':vcs_info:*' unstagedstr 'M'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{11}%r'
zstyle ':vcs_info:*' enable svn git

RPROMPT='$vcs_info_msg_0_'


# ssh_zsh() for remote systems that are bash-login based

autoload -U compinit
compinit

_ssh_zsh_config_hosts=($(grep '^Host' ~/.ssh/config 2>/dev/null | sed -e 's/^Host//'))

_ssh_zsh () {
    local state

    _arguments '1: :->ssh_host'

    case $state in
        (ssh_host) _arguments "1:ssh_hosts:($_ssh_zsh_config_hosts)" ;;
    esac
}

ssh_zsh () {
    /usr/bin/ssh -Y -t $@ -- zsh
}

compdef _ssh_zsh ssh_zsh

_ssh_host_completion () {
    local h
    h=($_ssh_zsh_config_hosts $(grep -v '^#' /etc/hosts | awk '{print $2}'))
    if [[ $#h -gt 0 ]]; then
        for x in ssh scp sftp rsync; do
            zstyle ":completion:*:$x:*" hosts $h
        done
    fi
}

_ssh_host_completion


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
            PROMPT=$'$PR_BLACK%n@%m$PR_RESET:$PR_BLUE%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))%# '
            ;;

        SunOS)
            alias ls='ls --color=auto'
            alias grep='grep --color=auto'
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

alias htop='sudo -E htop'

alias perlfreq="sudo dtrace -qZn 'sub-entry { @[strjoin(strjoin(copyinstr(arg3),\"::\"),copyinstr(arg0))] = count() } END {trunc(@, 10)}'"

alias perlop="sudo dtrace -qZn 'sub-entry { self->fqn = strjoin(copyinstr(arg3), strjoin(\"::\", copyinstr(arg0))) } op-entry /self->fqn != \"\"/ { @[self->fqn] = count() } END { trunc(@, 3) }'"


# presumes a running emacs-server

emac () {
    local args; args=()
    local nw=false
    local running=false
    # check if emacsclient is already running
    pgrep -U $(id -u) emacsclient > /dev/null && running=true

    # check if the user wants TUI mode
    local arg;
    for arg; do
    	if [ "$arg" = "-nw" ] || [ "$arg" = "-t" ] || [ "$arg" = "--tty" ]
        then
            nw=true
    	fi
    done

    # if called without arguments - open a new gui instance
    if [ "$#" -eq "0" ] || [ "$running" != true ]; then
        args+=(-c) # open emacsclient in a new frame
    fi
    if [ "$#" -gt "0" ]; then
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


_oci_pre_sync () {
    [ -z "$OCI_AD[$region]" ] || return 1
    echo Preparing $region with $ad Availability Domains.
    echo -n Step 1. Intitialize ssh.
    for id in {1..$ad}
    do
        echo Connecting to $OCI_HOST_PREFIX-$id.$region...
        ssh $OCI_HOST_PREFIX-$id.$region hostname
    done
    echo Step 2. Attach ISCSI devices manually, using details from OCI console.
    for id in {1..$ad}
    do
        echo Opening root shell on $OCI_HOST_PREFIX-$id.region...
        ssh $OCI_HOST_PREFIX-$id.$region sudo -s
    done
    echo -n Step 3. Create HApool, service accounts, and permissions.
    for id in {1..$ad}
    do
        for user in svn httpd ssh bb-master bb-worker
        do
            ssh $OCI_HOST_PREFIX-$id.$region sudo useradd -m -g 1 $user
        done
        ssh $OCI_HOST_PREFIX-$id.$region sudo iscsiadm modify discovery --static enable
        ssh $OCI_HOST_PREFIX-$id.$region sudo zpool create HApool c2t0d0
        ssh $OCI_HOST_PREFIX-$id.$region sudo zfs create -o mountpoint=/x1 HApool/x1
        ssh $OCI_HOST_PREFIX-$id.$region sudo usermod -K defaultpriv=basic,net_privaddr opc
    done
    echo Pre-sync prep complete.
}


_oci_post_sync () {
    echo Adjusting and Reloading config files.
    sed -i -e "s/OCI_AD=[(]/OCI_AD=( [$region]=$ad/" ~joe/.zshenv
    sed -i -e 's/^(Host \*\.us-ashburn-1.*)$/\1 *.'$region'/' ~/joe/.ssh/config
    . ~/.zshenv

    echo Reinitializing ssh connection cache for $region.
    rm ~joe/.ssh/sockets/*.$region-22
    for i in {1..$ad}
    do
        ssh $OCI_HOST_PREFIX-$i.$region true
    done

    echo Delegating zfs permissions to httpd.
    for i in {1..$ad}
    do
        ssh $OCI_HOST_PREFIX-$i.$region sudo zfs allow -ld httpd create,mount,snapshot,clone,destroy HApool/x1/cms/wc
    done

    _oci_ship_region_zones $region

    echo Post-sync prep complete.
}

oci_refresh_region_zones () {
    local region=$1
    local ad=$OCI_AD[$region]
    local ZONES=( $(ls /system/zones) )

    for id in {1..$ad}
    do
        for zone in ${ZONES[@]}
        do
            ssh $OCI_HOST_PREFIX-$id.$region sudo zoneadm -z $zone shutdown
            ssh $OCI_HOST_PREFIX-$id.$region sudo zoneadm -z $zone detach
        done >/dev/null 2>&1

        for zone in ${ZONES[@]}
        do
            echo Refreshing $zone on $OCI_HOST_PREFIX-$id.$region
            sudo zonecfg -z $zone export | ssh $OCI_HOST_PREFIX-$id.$region sh -c 'cat > $zone.cfg'
            ssh $OCI_HOST_PREFIX-$id.$region sudo zonecfg -z $zone $zone.cfg
            ssh $OCI_HOST_PREFIX-$id.$region sudo zoneadm -z $zone attach
            ssh $OCI_HOST_PREFIX-$id.$region sudo zoneadm -z $zone boot
        done
    done
}

oci_ship_zone () {
    local zone=$1
    local LAST=$(realpath --relative-to ~joe ~joe/.zulu-last | sed -e 's/^\.zulu-//')
    local vol=VARSHARE/zones/$zone

    sudo zoneadm -z $zone shutdown
    sudo zoneadm -z $zone detach
    sudo zfs snapshot -r rpool/$vol@$LAST >/dev/null 2>&1

    for region ad in ${(kv)OCI_AD}
    do
        for id in {1..$ad}
        do
            ssh $OCI_HOST_PREFIX-$id.$region sudo zoneadm -z $zone shutdown
            ssh $OCI_HOST_PREFIX-$id.$region sudo zoneadm -z $zone detach

            sudo zfs send -rc rpool/$vol@$LAST | lzop -c | \
                ssh $OCI_HOST_PREFIX-$id.$region sudo sh -c "'/usr/local/bin/lzop -d | zfs receive -F rpool1/$vol'"
            sudo zonecfg -z $zone export | ssh $OCI_HOST_PREFIX-$id.$region sh -c "'cat > $zone.cfg'"
            ssh  $OCI_HOST_PREFIX-$id.$region sudo zonecfg -z $zone -f $zone.cfg
            ssh $OCI_HOST_PREFIX-$id.$region sudo zoneadm -z $zone attach
            ssh $OCI_HOST_PREFIX-$id.$region sudo zoneadm -z $zone boot
        done
    done

    sudo zoneadm -z $zone attach
    sudo zoneadm -z $zone boot

}

_oci_ship_region_zones () {
    local region=$1
    local ad=$OCI_AD[$region]
    local ZONES=( $(ls /system/zones) )
    local LAST=$(realpath --relative-to ~joe ~joe/.zulu-last | sed -e 's/^\.zulu-//')
    local vol=rpool/VARSHARE/zones
    local target_vol=rpool1/VARSHARE/zones

    for zone in ${ZONES[@]}
    do
        sudo zoneadm -z $zone shutdown
        sudo zoneadm -z $zone detach
    done

    sudo zfs snapshot -r $vol@$LAST

    for id in {1..$ad}
    do
        for zone in ${ZONES[@]}
        do
            ssh $OCI_HOST_PREFIX-$id.$region sudo pkg install uvfs udfs diagnostic/cpu-counters service/file-system/nfs
            ssh $OCI_HOST_PREFIX-$id.$region sudo zoneadm -z $zone shutdown
            ssh $OCI_HOST_PREFIX-$id.$region sudo zoneadm -z $zone detach
        done >/dev/null 2>&1
        echo Shipping $vol to $OCI_HOST_PREFIX-$id.$region ...
        sudo zfs send -rc $vol@$LAST | lzop -c | ssh $OCI_HOST_PREFIX-$id.$region sudo sh -c "'/usr/local/bin/lzop -d | zfs receive -F $target_vol'"
        for zone in ${ZONES[@]}
        do
            sudo zonecfg -z $zone export | ssh $OCI_HOST_PREFIX-$id.$region sh -c "'cat > $zone.cfg'"
            ssh $OCI_HOST_PREFIX-$id.$region sudo zonecfg -z $zone -f $zone.cfg
            ssh $OCI_HOST_PREFIX-$id.$region sudo zoneadm -z $zone attach
            ssh $OCI_HOST_PREFIX-$id.$region sudo zoneadm -z $zone boot
        done
    done

    sudo zfs destroy -r $vol@$LAST

    for zone in ${ZONES[@]}
    do
        sudo zoneadm -z $zone attach
        sudo zoneadm -z $zone boot
    done
    echo All zones synced to $region.
}

oci_initialize_region () {
    local region=$1
    local ad=$2
    local LAST=$(realpath --relative-to ~joe ~joe/.zulu-last | sed -e 's/^\.zulu-//')
    [ -n "$region$ad" ] || return 1

    _oci_pre_sync

    sed -i -e "s/ \\[$region\\]=$ad//g" ~joe/.zshenv
    sudo rm -rf /x1/httpd/cores/*
    for volume in ${ZFS_EXPORTS[@]}
    do
        local vol=${volume#*/}
        local dst_mount=$vol
        local dst_pool=HApool
        if echo $vol | grep VARSHARE
        then
            dst_mount=$(echo $vol | sed s/VARSHARE/system/)
            dst_pool=rpool1
        fi

        echo Syncing /$dst_mount ...
        for id in {1..$ad}
        do
            sudo zfs snapshot $volume@$LAST >/dev/null 2>&1
            (sudo zfs send -rc $volume@$LAST | gzip | ssh $OCI_HOST_PREFIX-$id.$region sudo sh -c "' >/dev/null 2>&1; [ /$fs = /etc/mail ] && rm -rf /etc/mail >/dev/null 2>&1; zfs create -p $dst_pool/$vol >/dev/null 2>&1; gzip -d | zfs receive -F -o mountpoint=/$dst_mount $dst_pool/$vol && [ /$fs = /etc/svc/manifest/site ] && svccfg import /$dst_mount'"; \
             echo Done with /$fs on $OCI_HOST_PREFIX-$id.$region: zfs receive exit status=$?.) &
        done
        wait
    done

    _oci_post_sync

    echo "All set: $region initialized to $LAST."
}

oci_release () {
    local slice="${1-}"
    local ZULU=$(date -Iseconds | tr '+' 'Z')
    local LAST=$(realpath --relative-to ~joe ~joe/.zulu-last | sed -e 's/^\.zulu-//')
    local ZONES=( $(ls /system/zones) )
    [ -n "$LAST" ] || return 1

    sudo -u httpd /x1/cms/webgui/garbage_collector.pl 0 >/dev/null

    for zone in ${ZONES[@]}
    do
        sudo zoneadm -z $zone shutdown
        sudo zoneadm -z $zone detach
    done

    for region ad in ${(kv)OCI_AD}
    do
        for id in {1..$ad}
        do
            [ -z "$slice" -o $slice -eq $id ] || continue
            echo Upgrading $OCI_HOST_PREFIX-$id.$region...
            for zone in ${ZONES[@]}
            do
                ssh $OCI_HOST_PREFIX-$id.$region sudo zoneadm -z $zone shutdown
                ssh $OCI_HOST_PREFIX-$id.$region sudo zoneadm -z $zone detach
            done >/dev/null 2>&1
            for volume in ${ZFS_EXPORTS[@]}
            do
                local vol=${volume#*/}

                local dst_pool=HApool
                if echo $vol | grep -q VARSHARE
                then
                     dst_pool=rpool1
                fi

                local TMPFILE=/tmp/oci-$(basename $vol)-$ZULU.zfs.lzo
                sudo zfs snapshot -r $volume@$ZULU >/dev/null 2>&1

                [ -f $TMPFILE ] || sudo zfs send -RcI $LAST $volume@$ZULU | lzop -c > $TMPFILE
                scp $TMPFILE $OCI_HOST_PREFIX-$id.$region:$TMPFILE && ssh $OCI_HOST_PREFIX-$id.$region sudo sh -c "'/usr/local/bin/lzop -d <$TMPFILE | zfs receive -F $dst_pool/$vol && rm $TMPFILE'" || return $?
                if [ /$vol = /etc/svc/manifest/site ]
                then
                    for svc in ${OCI_SITE_SVCS[@]}
                    do
                        ssh $OCI_HOST_PREFIX-$id.$region sudo svcadm restart site/$svc
                    done
                fi
            done

            for zone in ${ZONES[@]}
            do
                ssh $OCI_HOST_PREFIX-$id.$region sudo zoneadm -z $zone attach
                ssh $OCI_HOST_PREFIX-$id.$region sudo zoneadm -z $zone boot
            done >/dev/null 2>&1

            echo $OCI_HOST_PREFIX-$id.$region upgrade complete.
        done
    done

    for zone in ${ZONES[@]}
    do
        sudo zoneadm -z $zone attach
        sudo zoneadm -z $zone boot
    done

    touch ~joe/.zulu-$ZULU
    if [ -z "$slice" ]
    then
        ln -s -f $(realpath --relative-to ~joe ~joe/.zulu-last) ~joe/.zulu-rollback
        ln -s -f .zulu-$ZULU ~joe/.zulu-last
    fi
    rm /tmp/oci-*
    echo "Released $ZULU."
}

oci_ship_crons () {
    for region ad in ${(kv)OCI_AD}
    do
        for id in {1..$ad}
        do
            for file in root httpd
            do
                sudo cat /var/spool/cron/crontabs/$file | perl -ple 's/sleep 0/"sleep " . int rand 100/e' | ssh $OCI_HOST_PREFIX-$id.$region sudo sh -c "'cat > /var/spool/cron/crontabs/$file && svcadm restart cron'" || return $?
            done
        done
    done
}

oci_region_sudo () {
    local region=$1
    shift
    local ad=$OCI_AD[$region]
    for i in {1..$ad}
    do
        ssh $OCI_HOST_PREFIX-$i.$region sudo "$@"
    done
}


oci_svcs_region_action () {
    local region=$1
    local action=${2-restart}
    local ad=$OCI_AD[$region]
    for i in {1..$ad}
    do
        for svc in ${OCI_SITE_SVCS[@]}
        do
            ssh $OCI_HOST_PREFIX-$i.$region sudo svcadm $action site/$svc
            sleep 10
        done
    done
}

oci_rollback () {
    local TARGET=$(realpath --relative-to ~joe ~joe/.zulu-rollback | sed -e 's/^\.zulu-//')
    [ -n "$TARGET" ] || return 1
    for region ad in ${(kv)OCI_AD}
    do
        for id in {1..$ad}
        do
            for volume in ${ZFS_EXPORTS[@]}
            do
                local fs=${volume#*/}
                ssh $OCI_HOST_PREFIX-$id.$region sudo zfs rollback -R HApool/$fs@$TARGET
            done
        done
        oci_svcs_region_action $region restart
    done

    echo Rolled back from $(realpath --relative-to ~joe ~joe/.zulu-last | sed -e 's/^\.zulu-//') to $TARGET.
    rm $(realpath ~joe/.zulu-last)
    ln -s -f .zulu-$TARGET ~joe/.zulu-last
    TARGET=$(echo ~joe/.zulu-2* | tr ' ' '\n' | tail -n 2 | head -n 1 | sed -e 's/^.*\.zulu-//')
    ln -s -f .zulu-$TARGET ~joe/.zulu-rollback
}

oci_tail_logs () {
    local kind=${1-access}
    for region ad in ${(kv)OCI_AD}
    do
        for id in {1..$ad}
        do
            ssh $OCI_HOST_PREFIX-$id.$region /usr/local/bin/tail -F /x1/logs/httpd/${kind}_log | grep -Ev "Go|libwww" &
        done
    done
    wait
}

oci_upgrade_region () {
    local region=$1
    for ad in $OCI_AD[$region]
    do
        for i in {1..$ad}
        do
            ssh $OCI_HOST_PREFIX-$i.$region sudo pkg set-publisher -G "'*'" -g "$PKG_REPOS" solaris
            ssh $OCI_HOST_PREFIX-$i.$region sudo pkg refresh
            ssh $OCI_HOST_PREFIX-$i.$region sudo sh -c "'pkg update && reboot'"
            echo $OCI_HOST_PREFIX-$i.$region upgraded - rebooting.
            sleep 100
        done
    done


}
