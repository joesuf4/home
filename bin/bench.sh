#!/usr/local/bin/zsh

H2_OPTS=(-n 100 -m)
declare -A URL_ENC
declare -A RESULTS

URL_ENC=(
    wordpress.com                                     gzip
    www.allstate.com                                  gzip
    www.pagecloud.com/blog                            br
    www.netlify.com/blog                              br
    www.sunstarsys.com/js/jquery.min                  br
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

report "Ping(RTT)"

for url in ${(k)URL_ENC}
RESULTS[$url]=$(curl -s -H "Accept-Encoding: $URL_ENC[$url]" https://$url | wc -c)

DIVISOR=20000
report "Content-Length(B)"

for i in 1 5 10 25
do
    for url in ${(k)URL_ENC}
    RESULTS[$url]=$(h2load $H2_OPTS $i -H "Accept-Encoding: $URL_ENC[$url]" https://$url | awk -F '[s, ]' '/^finished/ {print $4}')

    report "h2load $H2_OPTS $i"
done
