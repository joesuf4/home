#!/usr/bin/env bash
# SPDX License Identifier: Apache License 2.0
#
# Parallel Find Filename Xargs Grep (pffxg).
#
# Usage:
#
# $0 [ --version | --help | `ag` file extension class | --all | --unzip \
#     | --conf* $PFFXG_CONF \
#     | --list* | --no-* \
#     | [--args|--files|--batch] $PFFXG_ARGS \
#     | --cmd $PFFXG_CMD \
#     | [--workers|--jobs] $PFFXG_WORKERS \
#     | --max* $PFFXG_MAX_STATUS \
#     | --cache $PFFXG_CACHE [--re*]? [--compress]? \
#     | --level $PFFXG_LEVEL \
#     | --compressor $PFFXG_COMPRESSOR \
#     | [--excl*|--ignore] $PFFXG_EXCLUSIONS]* [$filename|--] ${cmd_args[@]}
#
# $filename is passed to the -name argument of find(1), so it can be a glob
# pattern.  The remainder of "$@" are arguments to be passed to $PFFXG_CMD,
# which defaults to grep(1) with a few additional --prefixed arguments.
#
# Special case parasitic behavior for `ag` file extension classes:
#     This script treat unrecognized '--' prefixed args as
#     file extension classes within `ag` and will limit the search
#     to files in those extension classes.  The default behavior
#     is to search the entire list of extension classes as a whitelist.
#     If you really want all files searched (including binary files), pass
#     the --all flag instead.

version="2.2"

# Changes with 2.2:
#
# - 'convenience' argument preprocessing
#
#
# Changes with 2.1:
#
# - reliability and sanity-check features for compressed cache users
#
#
# Changes with 2.0:
#
# - consolidated both pipelines into one for better CPU utilization
# - introduce PFFXG_CACHE (--cache) w/ optional compression, etc.
# - keep internal copy of `ag --list-file-types` for future "modernization".
# - added 'bp' extension to 'make' file extension class
# - introduce PFFXG_CONF optional (sourced) shell config file.
#
#
# Changes with 1.2:
#
# - Microsoft Windows 10 support
# - '--all' preserves old (unlimited file extension search) behavior
# - default PFFXG_ARGS increased from 500 to 10000
# - default PFFXG_CMD appended --ignore-case (for `ag` compat)
# - --ignore alias for --exclude (for `ag` compat)
# - --unzip decompresses range of types (including .core.lzo) on the fly
# - migrate script to bash for export -f (can't export fcns in sh)
# - implement "!"-prefix for $filename
#
#
# Changes with 1.1:
#
# - Borrows ag file extension classes
# - Adds --* args largely to override env vars

# customizable env vars:

# Fully-Qualified Path to (optional) config file
: ${PFFXG_CONF:=~/.pffxg.conf}
# $PWD directories to fully ignore
: ${PFFXG_EXCLUSIONS:=out release prebuilts kernel external hardware}
# The default grep(1) command plus appropriate base arguments
: ${PFFXG_CMD:=grep --color=always --with-filename --line-number --ignore-case}
# Tolerable exit status threshold for $PFFXG_CMD
: ${PFFXG_MAX_STATUS:=1}
# Maximum number of files to list on $PFFXG_CMD command line
: ${PFFXG_ARGS:=10000}
# File extensions to limit the search to; do not prefix these with .'s
: ${PFFXG_EXTENSIONS:=}
# Maximum number of concurrent jobs to run at any given moment
: ${PFFXG_WORKERS:=$(awk "BEGIN { printf \"%d\", 1 + sqrt($(nproc)) }")}
# Location of cache directory
: ${PFFXG_CACHE:=}
# You can enable/disable cache compression at any time and the cache will adjust (with a --refresh)
: ${PFFXG_COMPRESS:=}
# Cache compression level (happens in background so can be relatively expensive)
: ${PFFXG_LEVEL:=7}
# Compression prefix (lzop -U is faster than gzip is faster than pigz here)
: ${PFFXG_COMPRESSOR:=lzop -U}


# source config file if present

[ -f "$PFFXG_CONF" ] && . "$PFFXG_CONF"


# internal variables

filename=""
temp_dir="$(mktemp -d ${TMPDIR:-/tmp}/pffxg-XXXXXX)"
my_exclusions="[.]repo [.]git [.]svn"
exit_loop=0
usage_head=31
usage_tail=27
env_head=93
env_tail=24
all=0
unzip=0
not=""
refresh=""
cache_compression_extension="gz"
set_and_shift_cmd_args='cmd_args=(); while [ "$1" != "--" ]; do cmd_args+=("$1"); shift; done; shift'
original_argument_count="$#"
list_active_extensions=0


# $temp_dir cleanup issues

function cleanup () {
    rm -rf $temp_dir
}

trap cleanup EXIT

for signal in INT TERM QUIT HUP; do
    trap 'cleanup; trap - '$signal' EXIT; kill -'$signal' $$' $signal
done


# Copy of `ag --list-file-types` we will maintain ourselves since this list is
# kind of outdated for our Android trees.

declare -A extension_types=(
    [actionscript]="as  mxml"
    [ada]="ada  adb  ads"
    [asciidoc]="adoc  ad  asc  asciidoc"
    [asm]="asm  s"
    [batch]="bat  cmd"
    [bitbake]="bb  bbappend  bbclass  inc"
    [bro]="bro  bif"
    [cc]="c  h  xs"
    [cfmx]="cfc  cfm  cfml"
    [chpl]="chpl"
    [clojure]="clj  cljs  cljc  cljx"
    [coffee]="coffee  cjsx"
    [cpp]="cpp  cc  C  cxx  m  hpp  hh  h  H  hxx  tpp"
    [crystal]="cr  ecr"
    [csharp]="cs"
    [css]="css"
    [cython]="pyx  pxd  pxi"
    [delphi]="pas  int  dfm  nfm  dof  dpk  dpr  dproj  groupproj  bdsgroup  bdsproj"
    [dot]="dot  gv"
    [ebuild]="ebuild  eclass"
    [elisp]="el"
    [elixir]="ex  eex  exs"
    [elm]="elm"
    [erlang]="erl  hrl"
    [factor]="factor"
    [fortran]="f  f77  f90  f95  f03  for  ftn  fpp"
    [fsharp]="fs  fsi  fsx"
    [gettext]="po  pot  mo"
    [glsl]="vert  tesc  tese  geom  frag  comp"
    [go]="go"
    [groovy]="groovy  gtmpl  gpp  grunit  gradle"
    [haml]="haml"
    [handlebars]="hbs"
    [haskell]="hs  lhs"
    [haxe]="hx"
    [hh]="h"
    [html]="htm  html  shtml  xhtml"
    [ini]="ini"
    [ipython]="ipynb"
    [jade]="jade"
    [java]="java  properties"
    [js]="es6  js  jsx  vue"
    [json]="json"
    [jsp]="jsp  jspx  jhtm  jhtml  jspf  tag  tagf"
    [julia]="jl"
    [kotlin]="kt"
    [less]="less"
    [liquid]="liquid"
    [lisp]="lisp  lsp"
    [log]="log"
    [lua]="lua"
    [m4]="m4"
    [make]="Makefiles  mk  mak  bp"
    [mako]="mako"
    [markdown]="markdown  mdown  mdwn  mkdn  mkd  md"
    [mason]="mas  mhtml  mpl  mtxt"
    [matlab]="m"
    [mathematica]="m  wl"
    [md]="markdown  mdown  mdwn  mkdn  mkd  md"
    [mercury]="m  moo"
    [nim]="nim"
    [nix]="nix"
    [objc]="m  h"
    [objcpp]="mm  h"
    [ocaml]="ml  mli  mll  mly"
    [octave]="m"
    [org]="org"
    [parrot]="pir  pasm  pmc  ops  pod  pg  tg"
    [perl]="pl  pm  pm6  pod  t"
    [php]="php  phpt  php3  php4  php5  phtml"
    [pike]="pike  pmod"
    [plist]="plist"
    [plone]="pt  cpt  metadata  cpy  py  xml  zcml"
    [proto]="proto"
    [puppet]="pp"
    [python]="py"
    [qml]="qml"
    [racket]="rkt  ss  scm"
    [rake]="Rakefile"
    [restructuredtext]="rst"
    [rs]="rs"
    [r]="R  Rmd  Rnw  Rtex  Rrst"
    [rdoc]="rdoc"
    [ruby]="rb  rhtml  rjs  rxml  erb  rake  spec"
    [rust]="rs"
    [salt]="sls"
    [sass]="sass  scss"
    [scala]="scala"
    [scheme]="scm  ss"
    [shell]="sh  bash  csh  tcsh  ksh  zsh  fish"
    [smalltalk]="st"
    [sml]="sml  fun  mlb  sig"
    [sql]="sql  ctl"
    [stylus]="styl"
    [swift]="swift"
    [tcl]="tcl  itcl  itk"
    [tex]="tex  cls  sty"
    [tt]="tt  tt2  ttml"
    [toml]="toml"
    [ts]="ts  tsx"
    [twig]="twig"
    [vala]="vala  vapi"
    [vb]="bas  cls  frm  ctl  vb  resx"
    [velocity]="vm  vtl  vsl"
    [verilog]="v  vh  sv"
    [vhdl]="vhd  vhdl"
    [vim]="vim"
    [wix]="wxi  wxs"
    [wsdl]="wsdl"
    [wadl]="wadl"
    [xml]="xml  dtd  xsl  xslt  ent  tld  plist"
    [yaml]="yaml  yml"
)

# uniquify the extension_types values by doing a reverse map

declare -A extension_types_reversed
for ext in ${extension_types[@]}; do
    ((extension_types_reversed[$ext]++))
done

function print_extension_flags () {
    echo "Suppported file extension flags:"
    for type in $(echo "${!extension_types[@]}" | tr ' ' '\n' | sort); do
        echo -e "  --$type\n\t${extension_types[$type]}\n"
    done
}


# 'convenience' argument preprocessing

if [ $# -eq 1 -a "$1" != "--help" -a "$1" != "--version" -o $# -gt 1 -a "${1#-}" != "$1" -a "${1#--}" = "$1" ]; then
    set -- -- "$@"
fi


# simple argument processing

while [ "$exit_loop" -ne 1 ]; do
    arg="$1"

    if [ -n "$arg" ]; then
        shift
    fi

    case "$arg" in

        --version|--help)
            echo "Parallel Find Filename Xargs Grep (pffxg) VERSION $version"
            head -n $usage_head $0 | tail -n $usage_tail | sed -E 's/^#( |$)//'
            head -n $env_head $0 | tail -n $env_tail
            exit 0
            ;;

        --conf*)
            PFFXG_CONF="$1"
            [ -f "$PFFXG_CONF" ] && . "$PFFXG_CONF"
            shift
            ;;

        --list*)
            if [ "$original_argument_count" -eq 1 ]; then
                print_extension_flags
                exit 0
            fi
            list_active_extensions=1
            ;;

        --no-*)

            # strip '--no-' prefix, and then upper-case with '^^'
            # useful primarily with '--no-exclusions', '--no-compress', or '--no-cache'
            # to disable them if set (either by pffxg.sh defaults or in your bash environment)

            arg="${arg#--no-}"
            unset "PFFXG_${arg^^}"
            ;;

        --all)
            all=1
            ;;

        --args|--batch|--files)
            PFFXG_ARGS="$1"
            shift
            ;;

        --cmd)
            PFFXG_CMD="$1"
            shift
            ;;

        --workers|--jobs)
            PFFXG_WORKERS="$1"
            shift
            ;;

        --max*)
            PFFXG_MAX_STATUS="$1"
            shift
            ;;

        --excl*|--ignore)
            PFFXG_EXCLUSIONS="$1"
            shift
            ;;

        --unzip)

            # zipgrep is a mess (supports almost no standard grep args)
            # xzgrep supports the most extensions and the most grep args,
            # but like the rest is just a shell script that processes arguments
            # in its shell's 'for' loop, so don't expect major scalability here.

            unzip=1
            PFFXG_ARGS=1
            PFFXG_EXTENSIONS="gz gzip zip bz2 lz4 lzma lzo xz tar"
            PFFXG_CMD="grep -H -i -n"
            PFFXG_CACHE=""
            ;;

        --cache)
            PFFXG_CACHE="$1"
            shift
            ;;

        --re*)
            refresh=1
            ;;

        --compress)
            PFFXG_COMPRESS=1
            ;;

        --level)
            PFFXG_LEVEL="$1"
            shift
            ;;

        --compressor)
            PFFXG_COMPRESSOR="$1"
            shift
            ;;

        --)
            filename='*'
            exit_loop=1
            ;;

        --*)
            # strip '--' prefix and validate
            arg="${arg#--}"
            if [ -z "${extension_types[$arg]}" ]; then
                echo "pffxg.sh: INVALID argument: '--$arg' not recognized as a file extension type flag." >&2
                sleep 3
                print_extension_flags >&2
                exit 2
            fi
            PFFXG_EXTENSIONS+=" ""${extension_types[$arg]}"
            ;;

        !*)
            # strip '!' prefix
            filename="${arg#!}"
            not="!"
            exit_loop=1
            ;;

        *)
            filename="$arg"
            exit_loop=1
            ;;
    esac
done

if [ "$all" -eq 1 ]; then
    PFFXG_CACHE=
    PFFXG_EXTENSIONS=
elif [ -z "$PFFXG_EXTENSIONS" ]; then
    PFFXG_EXTENSIONS="${!extension_types_reversed[@]}"
fi

if [ "$list_active_extensions" -eq 1 ]; then
    echo -e "List of active file extensions:\n\t$PFFXG_EXTENSIONS"
    exit 0
fi

# create cache dir if configured and deal with subsequent compression settings

if [ -n "$PFFXG_CACHE" ]; then
    mkdir -p -m 0700 $PFFXG_CACHE
    mkdir -p "$PFFXG_CACHE$PWD"
    if ! df "$PFFXG_CACHE" | grep -Eq '^te?mpfs'; then
        echo "pffxg.sh: WARNING: cache base dir '$PFFXG_CACHE' not on tempfs!" >&2
    fi

    # work directly from the cache tree unless we are refreshing it
    if [ -z "$refresh" ]; then
        cd "$PFFXG_CACHE$PWD"
        if [ -n "$PFFXG_COMPRESS" ] && pgrep -P 1 -u $USER "$(echo "$PFFXG_COMPRESSOR" | cut -d' ' -f1)"; then
            echo "Compression still in progress from a prior run..."
            echo "Please wait for the above-listed processes to finish."
            exit 4
        fi
    fi
fi

if [ "$(echo "$PFFXG_COMPRESSOR" | cut -d' ' -f1)" = "lzop" ]; then
    cache_compression_extension="lzo"
fi


# argument compatibility/sanity check

if [ -n "$PFFXG_EXTENSIONS" -a "$unzip" -ne 1 -a -z "$not" ] && echo "$filename" | grep -vq '[*]$'; then
    echo "PFFXG_EXTENSIONS set, which conflicts with filename glob '$filename'" >&2
    echo 'Be sure you provided the true *-suffixed $filename argument right after your `ag`-style file extension classes!' >&2
    echo 'If you really intend to restrict the file extension to an exact match, please pass the --all argument on the command line!' >&2
    exit 2
fi


# named filters

function filter_exclusions () {
    grep -Ev "^($(echo $my_exclusions $PFFXG_EXCLUSIONS | tr ' ' '|'))\$"
}

function filter_extensions () {
    if [ -n "$PFFXG_EXTENSIONS" ]; then
        local compression_suffix=""

        # set compression_suffix if find's $PWD is a cache dir, not
        # the live tree. Only needed if the pre-existing cache dir used
        # compression, which is available to us via nonempty $PFFXG_COMPRESS.

        [ -n "$PFFXG_CACHE" -a -z "$refresh" -a -n "$PFFXG_COMPRESS" ] \
            && compression_suffix="[.]$cache_compression_extension"
        grep -E "[.]($(echo $PFFXG_EXTENSIONS | tr ' ' '|'))([.][^./]+)*$compression_suffix\$"
    else
        cat
    fi
}


# helper functions

function unzip_prefix () {
    # set (--unzip) grep prefix
    local prefix=""

    case "$(basename "$1")" in

        *.zip)
            prefix=zip
            ;;

        *.bz2)
            prefix=bz
            ;;

        *.gz|*.gzip|*.xz|*.lz4|*.lzo|*.lzma)
            prefix=xz
            ;;

        *.tar)
            prefix=ptar
            ;;

    esac

    echo $prefix
}

# Notes on bash variable munging used below for dealing with cache compression:
# ${@%$suffix} removes $suffix from the end of each item (if present) in $@.
# ${@/%/$suffix} appends $suffix to each item in $@.  The nice thing about all
# this is that it preserves the shell auto-quoting semantics of "$@", so we
# don't choke on any embedded whitespace in filename paths.

function process_cache () {

    if [ -n "$refresh" ]; then
        local cache_dir="$PFFXG_CACHE$PWD"

        # first we attempt to expand any compressed cache files for subsequent
        # `cp -u` mtime comparison check, to avoid unnecessary copying.

        cd "$cache_dir" || exit 255
        $PFFXG_COMPRESSOR -d -f -- "${@/%/.$cache_compression_extension}" 2>/dev/null
        cd "$OLDPWD"

        # back in live tree: copy to $cache_dir if newer, preserving parent dirs
        cp -u --parents -- "$@" "$cache_dir"

        # $cache_dir is $PWD for remainder of bash script;
        # $compression_suffix stays empty, since `find` walked the live tree
        # and the filenames xargs added to "$@" are from that tree walk (besides
        # we decompressed and destroyed any pre-existing compressed variants
        # in the cache tree a few lines up in this section).

        cd "$cache_dir" || exit 255
        [ -n "$PFFXG_COMPRESS" ] && trap '(compress_cache "$@" &)' EXIT
        return 0
    fi

    [ -z "$PFFXG_COMPRESS" ] && return 0
    compression_suffix=".$cache_compression_extension"

    # decompress, but keep compressed originals for future use
    $PFFXG_COMPRESSOR -d -f -k -- "$@"
    trap '(compress_cache "$@" &)' EXIT

}

function compress_cache () {
    # this runs double-forked the background; init(1) is its parent.
    if [ -n "$refresh" ]; then
        nice $PFFXG_COMPRESSOR -$PFFXG_LEVEL -f -- "$@"
    else
        nice rm -f -- "${@%$compression_suffix}"
    fi
}

function single_quote () {
    # Embed "$@" arguments in single quotes and escape embedded quotes
    # (echo is a bash built-in so has no limit to argument list size).
    # Note: this only runs twice per pffxg.sh invocation so we need correctness,
    # not speed.  It should only be processing the "cmd_args" passed on
    # the command line for pffxg.sh just after the filename glob.

    # Append a newline to each item in $@ and munge the quotes on each line.
    # The pipelined output trades newlines for single space characters, so
    # we wind up with a one-line output terminated by a space, not a newline.

    echo -en "" "${@/%/\\n}" \
        | sed -e "s/'/'\\\\''/g" -e "s/^ /'/" -e "s/\$/'/" | tr '\n' ' '
}


# bash exports

export refresh cache_compression_extension \
       PFFXG_EXTENSIONS PFFXG_CACHE PFFXG_COMPRESS PFFXG_COMPRESSOR PFFXG_LEVEL
export -f unzip_prefix filter_extensions process_cache compress_cache


# In this program, $PFFXG_WORKERS caps the concurrency of the xargs -P targets.

# In the following 2-stage pipeline, an `ls -A` initiated pipeline, ending in
# xargs -P, will spawn one find-job per $find_args entries of $PWD, each of
# which will create a second pipeline ending in xargs -P again, to spawn
# $PFFXG_CMD-jobs with up to $PFFXG_ARGS files on the command line. Each of
# those writes to a $$-named temp file for final collation and delivery without
# any mutexes in sight.
#
# The general trick we use to get "tainted" arguments safely incorporated into
# a bash -c "script" is to pass them along as "$@" arguments to the bash command
# after the -c "script", with a first passed argument being "--" to serve as a
# marker that the bash CLI arguments are over. It also serves as "$0" for the
# "script" name. The remainder of the argument list to the script is either
# populated by hand after handing off to the single_quote() function, or just
# additionally by xargs itself.
#
# There is an intricate dance here between different quoting contexts-
# take your time with the parsing effort; most shell editors will get it right.
# Stuff like 'foobar "$@" "'"$(echo "$@")"'"bat'"quux'\"\$@\"' $Z" is about the
# level of chicanery in what lies ahead.

find_args="$(ls -A | filter_exclusions | wc -l | awk "{printf \"%d\", \$1 / $PFFXG_WORKERS + 1}")"

ls -A | filter_exclusions | xargs -r -d '\n' -P $PFFXG_WORKERS -n $find_args bash -c \
    'find "$@" -type f '$not' -name '"$(single_quote "$filename")"' \
    | grep -vE "/('"$(echo $my_exclusions | tr ' ' '|')"')/" \
    | filter_extensions \
    '" switched from single to double quotes in outer bash script \
    | xargs -r -d '\\n' -P $PFFXG_WORKERS -n $PFFXG_ARGS bash -c \
    '
      # the inner bash search (meat of the program: the 'grep' script)
      $set_and_shift_cmd_args
      compression_suffix=
      [ -n \"$PFFXG_CACHE\" ] && process_cache \"\$@\"
      \$([ $unzip -eq 1 ] && unzip_prefix \"\$1\")$PFFXG_CMD \
          \"\${cmd_args[@]}\" -- \"\${@%\$compression_suffix}\" >> $temp_dir/\$\$
      [ \$? -gt $PFFXG_MAX_STATUS ] && exit 255
    ' \
    -- $(single_quote "$@") --
    # back to outer bash script (the 'find' pipeline ended on previous line)
    [ \$? -gt 123 ] && exit 255
    " --


# check xargs exit status for fatal errors that caused early termination (abort)

rc=$?

if [ "$rc" -gt 123 ]; then
    cmd=$(echo "$PFFXG_CMD" | cut -d' ' -f1)
    echo "pffxg.sh: [$PFFXG_CMD $@] FATALITY: exit status exceeded $PFFXG_MAX_STATUS." >&2
    echo -e "See the $cmd manpage for additional help.\n" >&2
    $cmd --help >&2
    exit $rc
fi

cat $temp_dir/*
exit 0
