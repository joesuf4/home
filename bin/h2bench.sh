#!/usr/bin/env -S zsh -i

echo "$0: https://github.com/joesuf4/home/blob/master/bin/h2bench.sh"

H2_OPTS=(-n 100 -m)
[ -f ~/.h2cookie ] && . ~/.h2cookie

declare -A URL_ENC
declare -A RESULTS

report () {
    echo $1
    echo ----------------------
    for k v in ${(kv)RESULTS}
    do
        echo ${k%%/*} $v
    done | top_10
    RESULTS=()
    echo
}

benchmark () {
    echo
    echo "             $1, Compressed HTTP/2 Page Delivery Benchmarks"
    echo

    for url in ${(k)URL_ENC%%/*}
    RESULTS[$url]=$(ping -c 1 $url | awk -F '[/]' '$5 {print $5 ms}')

    report "Ping(RTT) in ms"

    for url in ${(k)URL_ENC}
    RESULTS[$url]=$(curl -s -H $H2_COOKIE -H "Accept-Encoding: $URL_ENC[$url]" "https://$url" | wc -c)

    report "Content-Length in B"

    for i in 1 5 10 25
    do
        for url in ${(k)URL_ENC}
        RESULTS[$url]=$(h2load -H $H2_COOKIE $H2_OPTS $i -H "Accept-Encoding: $URL_ENC[$url]" "https://$url" | awk -F '[s, ]' '/^finished/ {print $4}')

        report "h2load $H2_OPTS $i -- seconds"
    done
}

# static urls w/ compression

URL_ENC=(
    # wordpress is nginx (doesn't support brotli)
    wordpress.com                                     gzip

    # allstate is akamai (doesn't support brotli)
    www.allstate.com                                  gzip

    # newyorker is fastly (doesn't support brotli)
#    www.newyorker.com/journey/compiler/build-c3bb2534453a3d9e58c98c26e67b6204.js gzip

    # pagecloud is cloudflare
    www.pagecloud.com/blog                            br

    # netlify is AWS:custom
    www.netlify.com/blog                              br

    # sunstarsys is OCI:httpd/2.4
    www.sunstarsys.com/editor.md/js/jquery.min.js   gzip
)

benchmark Static

# dynamic urls w/ compression

URL_ENC=(
    # simple modperl registry script
    'www.sunstarsys.com/dynamic/enquiry.pl?lang=.en' br

    # tiny admin bar graphic
    'joesuf4.wordpress.com/wp-includes/charts/admin-bar-hours-scale-2x.php?masterbar=1&s=184609717' gzip
)

benchmark Dynamic
