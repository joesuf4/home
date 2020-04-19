# Home directory git repo

I use this primarily to distribute my home directory's essential
files across several servers.  The sources are found on my laptop, are
pushed to my github account, and from there are cloned to my home directory
across various hosts, typically with ansible.

Most of my worktime is spent in zsh running inside a screen session on
my laptop, and that's reflected in my config.  Obviously I use emacs
a lot, and the .emacs file contains a few essential elpa/melpa packages;
I couldn't get by without some of them.

The configs are messy and not really aimed at public distribution, but
I'd be happy to consider appropriate patches if there's something you'd
like to see me make use of.

The reusable scripts are over in the bin/ dir.  I have an "strace" drop-in
for my MBP for `sudo htop` to use for tracing processes. The only nontrivial
script is `bin/pffxg.sh` - a parallelized recursive grep - targeting *very*
large source trees typical for embedded development work.  I've included some
documentation for it below.

## Parallel Find Filename Xargs Grep (~/bin/pffxg.sh)

### TLDR; 4x faster than any other parallelized-recursive-grep solution.

Modeled on the API for The Silver Surfer (`ag`), `pffxg.sh` is a simpler, faster,
and more feature-rich product in one-tenth the source code line count.  Comes
with native support for a tmpfs-based (optionally lzo-compressed) cache, which
provides *consistency* in search result efficiency unavaible in other similar
products.  Portable to any platform that supports bash + standard GNU `fileutils`,
plus `lzop` if you need a cache, including Windows 10 and Mac OSX.


To search `pwd` for `foo`:

```
    % pffxg.sh foo
```

### Usage

```
   % pffxg.sh --help
   Parallel Find Filename Xargs Grep (pffxg) VERSION 2.2

   Usage:

   $0 [ --version | --help | `ag` file extension class | --all | --unzip \
       | --conf* $PFFXG_CONF
       | --list* | --no-* \
       | [--args|--files|--batch] $PFFXG_ARGS \
       | --cmd $PFFXG_CMD \
       | [--workers|--jobs] $PFFXG_WORKERS \
       | --max* $PFFXG_MAX_STATUS \
       | --cache $PFFXG_CACHE [--re*]? [--compress]?
       | --level $PFFXG_LEVEL \
       | --compressor $PFFXG_COMPRESSOR \
       | [--excl*|--ignore] $PFFXG_EXCLUSIONS]* [$filename|--] ${cmd_args[@]}

   $filename is passed to the -name argument of find(1), so it can be a glob
   pattern.  The remainder of "$@" are arguments to be passed to $PFFXG_CMD,
   which defaults to grep(1) with a few additional --prefixed arguments.

   Special case parasitic behavior for `ag` file extension classes:
       This script treat unrecognized '--' prefixed args as
       file extension classes within `ag` and will limit the search
       to files in those extension classes.  The default behavior
       is to search the entire list of extension classes as a whitelist.
       If you really want all files searched (including binary files), pass
       the --all flag instead.


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
```

### Customizations (~/.pffxg.conf)

```
    % cat .pffxg.conf
    PFFXG_WORKERS=3
    PFFXG_EXCLUSIONS=.ccls-cache
    PFFXG_ARGS=1000
    PFFXG_CMD="grep --color=always --with-filename --line-number"
```

These customizations are for my 4-core (+HyperThreading) MacBook Pro.  The
settings are straightforward: I don't want `grep` to `--ignore-case`, so I
disable that option in `PFFXG_CMD`.  I don't have enough RAM (8 GB) to take
advantage of a (compressed) cache, so caching isn't enabled here (`lzop` is
available from `brew install lzop`, which is the only non-trivial dependency
for `pffxg.sh`).


### Emacs Interface

Copy and paste into `~/.emacs` to install the `pffxg` elisp function into emacs:

```
    (defvar pffxg-command "pffxg.sh ")
    (defun pffxg (command-args)
      (interactive
       (progn
         (list (read-from-minibuffer "Find filenames and grep them (like this): "
		        pffxg-command nil nil
				'grep-find-history))))
      (let ((null-device nil))		; see grep
        (grep command-args)))
```
