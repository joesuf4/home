# Home directory git repo

I use this primarily to distribute my home directory's essential
files across several servers.  The sources are found on my laptop, are
pushed to my github account, and from there are cloned to my home directory
across various hosts, typically with ansible.

Most of my worktime is spent in zsh running inside a pty-driven (@SunStarSys
has the `pty` git repo) tmux session on my laptop, and that's reflected in
my config.  Obviously I use emacs a lot, and the .emacs file contains a few
essential elpa/melpa packages; I couldn't get by without some of them.  Of
particular note is `ccls`, which is an excellent piece of software to drive
clang-based LSP IDE apps (like emacs).

The configs are messy and not really aimed at public distribution, but
I'd be happy to consider appropriate patches if there's something you'd
like to see me make use of.

The reusable (static) binaries and scripts are over in the bin/ dir. The were
compiled against Ubuntu 20.04's glibc, and most will not work on newer glibc
operating systems. The `screen` binary should be suid root for session reattachment
to have any hope of working. The only nontrivial script is `bin/pffxg.sh` -
a parallelized recursive grep - targeting *very* large source trees typical
for embedded development work.  I've included some primitive documentation for
it below the next section.

## Ansible-friendly Linter Framework

Copy and paste linter.{rc,sh} into your local repo, run `./linter.sh install`,  enjoy.
Workable for any PR/MR-based workflow and programming language framework imaginable.

# WSL Specs

I have python, emacs, mingw+git, and Docker Desktop locally installed in Windows
itself.  No Windows IDE/compiler whatsoever.  I launch an emacs daemon at
Windows Login Start Up, which I realistically only ever interact with via
the Ubuntu emacsclient symlink in ~/bin. All of my emacs-related LSP daemons
run within WSL itself, (via Ubuntu-clang-based custom software builds,) through
*.bat shims in ~winhome/bin.

I have no emacs install on Ubuntu; or any X11 client/server support for that
matter. I want a modern, DRM/4K+HDR enabled, secure, professional graphics/font
engines that are *promised* to work with modern hardware and backed up by a support
contract.  I insist on paying, or having my employer pay, Microsoft (or Apple,
once up on time) for the privilege.  X11 ain't that.  Not even close.

I keep `~winhome` under this git repo, as well as Ubuntu's `~` dir.  I have `.zshrc`
aliases to make synchronization via git push/pull trivial.  I rely on
Powershell for CLI Terminal sessions, and drop into my Ubuntu install via the
`wsl` command.

This is my preferred business environment to work within, and my employer gives
me amazing hardware and software support, so I don't need Windows admin privs to
get it done.  Fuck Apple, **this** is the cat's meow.

I have static builds of my Linux observability toolchain living in `~/bin` along
with a lot of other interesting things.  These are stand-alone tools that will
execute on any reasonably current Linux bare-metal host, or running Linux-based
container, or WSL (on a 5.x kernel built w/ CONFIG_IKHEADERS=yes in the config)
itself.

Update (12/22/2021): Experimenting with wslg on Windows 11 (X11 has no alpha
channel support yet).  Emacs for Windows has some pain points on WSL, so I may
need an attitude adjustment against my X11 loathing now the MS is in the game.

## Parallel Find Filename Xargs Grep (~/bin/pffxg.sh)

### TLDR; 4x faster than any other parallelized-recursive-grep solution.

Modeled on the API for The Silver Searcher (`ag`), `pffxg.sh` is a simpler, faster,
and more feature-rich product in one-tenth the source code line count.  Comes
with native support for a tmpfs-based (optionally lzo-compressed) cache, which
provides *consistency* in search result efficiency unavailable in other similar
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
    PFFXG_EXCLUSIONS='[.]ccls-cache [.]*[.]min[.][a-z]*'
```

My employer has me on a top-of-the-line Dell Precision 5550 i9 w/8 cores,
which presents as 16 CPU to WSL/Linux. Only WSL kernel builds, `eks report
global . -n 1000` and broad `pffxg.sh` searches, light up the fans (for 2-3
min tops). I've had it with "*.min.{js,css} as text-file" greps, so `pffxg.conf`
excludes them all.

### Emacs Interface

Copy and paste into `~/.emacs` to install the `pffxg` elisp function into emacs:

```
    (defvar pffxg-command "bash pffxg.sh ")
    (defun pffxg (command-args)
      (interactive
       (progn
         (list (read-from-minibuffer "Find filenames and grep them (like this): "
		        pffxg-command nil nil
				'grep-find-history))))
      (let ((null-device nil))		; see grep
        (grep command-args)))
```
