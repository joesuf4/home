#!/usr/bin/env zsh

echo "$0: https://github.com/joesuf4/home/blob/master/bin/h2bench.sh"

H2_OPTS=(-n 100 -m)
declare -A URL_ENC
declare -A RESULTS

URL_ENC=(
    # wordpress is nginx (doesn't support brotli)
    wordpress.com                                     gzip
    # allstate is akamai (doesn't support brotli)
    www.allstate.com                                  gzip
    # pagecloud is cloudflare
    www.pagecloud.com/blog                            br
    # netlify is AWS
    www.netlify.com/blog                              br
    # sunstarsys is OCI:httpd/2.4
    www.sunstarsys.com/js/jquery.min.js               br
)

report () {
    echo $@
    echo --------------------
    for k v in ${(kv)RESULTS}
    do
        echo ${k%%/*} $v
    done | perl -nale 'END{ printf "%20s: %s\n", $_, ("x" x ($h{$_} / '${DIVISOR-1}') . " $h{$_}") for sort {$h{$b} <=> $h{$a}} keys %h}
     tr/m//d && tr/.//d && s/^/./ for $F[1]; $h{$F[0]}=$F[1]'
    RESULTS=()
    unset DIVISOR
    echo
}


for url in ${(k)URL_ENC%%/*}
RESULTS[$url]=$(ping -c 1 $url | awk -F '[/]' '$5 {print $5}')

report "Ping(RTT) in ms"

for url in ${(k)URL_ENC}
RESULTS[$url]=$(curl -s -H "Accept-Encoding: $URL_ENC[$url]" https://$url | wc -c)

DIVISOR=1000
report "Content-Length in B"

for i in 1 5 10 25
do
    for url in ${(k)URL_ENC}
    RESULTS[$url]=$(h2load $H2_OPTS $i -H "Accept-Encoding: $URL_ENC[$url]" https://$url | awk -F '[s, ]' '/^finished/ {print $4}')

    report "h2load $H2_OPTS $i -- duration in s"
done
