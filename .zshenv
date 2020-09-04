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

declare -A OCI_AD
# do not edit the next line manually!
OCI_AD=( [de-frankfurt-1]=1 [us-ashburn-1]=1 [us-phoenix-1]=1 [au-sydney-1]=1 [bz-saopaolo-1]=1 )

OCI_SITE_SVCS=(http:apache24 markdownd svnwcsub watchdog)
OCI_HOST_PREFIX=HA-fileserver
OCI_ZPOOL=HApool

ZFS_TANK_EXPORTS=(tank/x1/cms tank/x1/svnpubsub tank/x1/httpd)
ZFS_RPOOL_EXPORTS=(rpool/usr/local rpool/etc/letsencrypt rpool/etc/mail rpool/etc/svc/manifest/site)
ZFS_EXPORTS=(${ZFS_TANK_EXPORTS[@]} ${ZFS_RPOOL_EXPORTS[@]})

PKG_REPOS=http://127.0.0.1:9999/

for p in /sbin /usr/bin /usr/cluster/bin /usr/local/bin $HOME/bin
do
    PATH="$p$(echo ":$PATH" | /usr/bin/sed -e "s|:$p||g")"
done

: ${TERM:=xterm-256color}
[ "$(uname)" = "SunOS" ] && TERM=xterm-256color
export TERM
