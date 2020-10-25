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
    # newyorker is fastly (doesn't support brotli)
    www.newyorker.com/prebid.min.js                   gzip
    # pagecloud is cloudflare
    www.pagecloud.com/blog                            br
    # netlify is AWS:custom
    www.netlify.com/blog                              br
    # sunstarsys is OCI:httpd/2.4
    www.sunstarsys.com/js/jquery.min.js               br
)

report () {
    echo $1
    echo --------------------
    for k v in ${(kv)RESULTS}
    do
        echo ${k%%/*} $v
    done | perl -nale 'END{ printf "%20s: %s\n", $_, ("x" x ($h{$_} / '${2-1}') . " $h{$_}") for sort {$h{$b} <=> $h{$a}} keys %h}
     tr/m//d && tr/.//d && s/^/./ for $F[1]; $h{$F[0]}=$F[1]'
    RESULTS=()
    echo
}

benchmark () {
    for url in ${(k)URL_ENC%%/*}
    RESULTS[$url]=$(ping -c 1 $url | awk -F '[/]' '$5 {print $5}')

    report "Ping(RTT) in ms"

    for url in ${(k)URL_ENC}
    RESULTS[$url]=$(curl -s $H2_COOKIE -H "Accept-Encoding: $URL_ENC[$url]" https://$url | wc -c)

    report "Content-Length in B" 1000

    for i in 1 5 10 25
    do
        for url in ${(k)URL_ENC}
        RESULTS[$url]=$(h2load $H2_COOKIE $H2_OPTS $i -H "Accept-Encoding: $URL_ENC[$url]" https://$url | awk -F '[s, ]' '/^finished/ {print $4}')

        report "h2load $H2_OPTS $i -- duration in s"
   done
}

echo "                 Static Page Delivery Benchmarks"
benchmark

URL_ENC=(
    www.sunstarsys.com/cgi-bin/enquiry.pl br
    'joesuf4.wordpress.com/wp-includes/charts/admin-bar-hours-scale-2x.php?masterbar=1&s=184609717' gz
)

echo "                Dynamic Page Delivery Benchmarks"
benchmark
