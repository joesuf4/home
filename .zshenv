export LANG=en_US.UTF-8
export LESSCHARSET=utf-8
export PAGER=less
export VISUAL="emacs"
export MOZILLA=firefox
export EDITOR="emacs"
export MANPATH="/usr/local/share/man:/usr/share/man"
export LDFLAGS="-L/usr/local/lib/amd64 -R/usr/local/lib/amd64 -L/usr/local/lib -R/usr/local/lib"
export CPPFLAGS="-I/usr/local/include"
export GIT_SSL_NO_VERIFY=1
declare -A OCI_AD;
# do not edit the next line manually!
OCI_AD=( [us-ashburn-1]=3 )
ZFS_EXPORTS=(rpool/usr/local rpool/etc/letsencrypt tank/x1/cms tank/x1/svnpubsub tank/x1/httpd rpool/etc/mail rpool/etc/svc/manifest/site)

for p in /usr/ccs/bin /opt/sfw/bin /usr/sfw/bin /usr/sbin /sbin /usr/cluster/bin /usr/local/bin $HOME/bin
do
    PATH="$p$(echo ":$PATH" | /usr/bin/sed -e "s|:$p||g")"
done

: ${TERM:=xterm-256color}
[ "$(uname)" = "SunOS" ] && TERM=xterm-256color
export TERM
