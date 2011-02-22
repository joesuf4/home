# Generated automatically from Makefile.in by configure.
# Makefile.in 
#
# Makefile for the GNU Emacs lisp library, PSGML

prefix = /usr
datadir = ${prefix}/share
lispdir = $(prefix)/share/emacs/site-lisp
infodir = ${prefix}/info
srcdir = .


EMACS = /usr/bin/emacs
XEMACS = xemacs


FLAGS = -batch -q -no-site-file -l ./psgml-maint.el
INSTALL = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644
SHELL = /bin/sh

compile:
	$(EMACS) $(FLAGS) -f psgml-compile-files

clean:
	-rm -f core `find . \( -name '*.orig' -o -name '*.rej' -o -name '*~' \
	       -o -name '*.bak' -o -name '#*#' -o -name '.*.orig' \
	       -o -name '.*.rej' -o -name '.SUMS' -o -size 0 \) -print` TAGS
	rm -f *.elc ;

all: 
	rm -f *.elc ; $(EMACS) $(FLAGS) -f psgml-compile-files


install: compile 
	$(SHELL) mkinstalldirs $(lispdir)
	for p in *.elc; do \
	  echo " $(INSTALL_DATA) $$p $(lispdir)/$$p"; \
	  $(INSTALL_DATA) $$p $(lispdir)/$$p; \
	done
	$(INSTALL_DATA) iso88591.map $(lispdir)

install-info:
	$(INSTALL_DATA) psgml.info $(infodir)
	$(INSTALL_DATA) psgml-api.info $(infodir)
	if $(SHELL) -c 'install-info --version' 2>&1 | grep Debian \
		>/dev/null ; then \
		install-info --info-dir=$(infodir) psgml.info; \
		install-info --info-dir=$(infodir) psgml-api.info; \
	else true; fi


xemacs:
	$(MAKE) EMACS=xemacs all

install-xemacs:
	$(MAKE) EMACS=xemacs install


# Tell versions [3.59,3.63) of GNU make not to export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:

# Makefile.in ends here
