SHELL       = /bin/sh
prefix      = /usr/local
exec_prefix = ${prefix}
bindir      = ${exec_prefix}/bin
libdir      = ${exec_prefix}/lib
srcdir      = .

top_builddir = ../
top_srcdir   = ..

GOSH = /usr/bin/gosh
GAUCHE_CONFIG = /usr/bin/gauche-config
INSTALL_TYPE = site
INSTALL = /usr/bin/install -c
WILIKI = $(GOSH) -I../src $(top_srcdir)/bin/wiliki

TEXTDIR = manual-ja
HTMLDIR = manual-html-ja

GENERATED = $(HTMLDIR)

all : 

$(HTMLDIR)/index.html :
	$(WILIKI) format manual-ja manual-html-ja

install :

clean :
	rm -rf core *~ manual-ja/*~ $(GENERATED)

distclean : clean
	rm -f Makefile

maintainer-clean :
	rm -f Makefile

check :

test :
