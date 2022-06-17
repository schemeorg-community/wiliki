GOSH = /usr/bin/gosh
GAUCHE_CONFIG = /usr/bin/gauche-config
INSTALL_TYPE = site
INSTALL = /usr/bin/install -c

SCMFILES = wiliki.scm \
	   wiliki/banned-content.scm \
	   wiliki/db.scm \
	   wiliki/edit.scm \
	   wiliki/format-scheme.scm \
	   wiliki/format.fixed.scm \
	   wiliki/format.hacked-to-work.scm \
	   wiliki/format.original.scm \
	   wiliki/log.scm \
	   wiliki/macro.scm \
	   wiliki/mcatalog.scm \
	   wiliki/pasttime.scm \
	   wiliki/rss.scm \
	   wiliki/rssmix.scm \
	   wiliki/settings-css.scm \
	   wiliki/settings-user-name.scm \
	   wiliki/settings.scm \
	   wiliki/util.scm \
	   wiliki/version.scm \
           wiliki/format.scm \
           wiliki/history.scm \
           wiliki/log.scm \
           wiliki/macro.scm \
           wiliki/mcatalog.scm \
           wiliki/pasttime.scm \
           wiliki/rss.scm \
           wiliki/rssmix.scm \
           wiliki/util.scm

TARGET = $(SCMFILES) wiliki/msgs.jp dbm/gdbm-ci.scm

GENERATED = wiliki/msgs.jp wiliki/version.scm

SCM_INSTALL_DIR = $(DESTDIR)`$(GAUCHE_CONFIG) --$(INSTALL_TYPE)libdir`

all : wiliki/msgs.jp wiliki/version.scm

wiliki/msgs.jp: wiliki/msgs.jp.euc
	$(GOSH) -ugauche.charconv -e '(call-with-input-file "wiliki/msgs.jp.euc" (lambda (in) (call-with-output-file "wiliki/msgs.jp" (cut copy-port in <> :unit (quote char)))) :encoding "euc-jp")' -Eexit

wiliki/version.scm : ../VERSION
	$(GOSH) ./gen-version ../VERSION > wiliki/version.scm

extract :
	$(GOSH) ./extract.scm wiliki/msgs.jp.euc $(SCMFILES)

install :
	if test ! -d $(SCM_INSTALL_DIR)/wiliki; then \
	  $(INSTALL) -d $(SCM_INSTALL_DIR)/wiliki; \
	fi
	if test ! -d $(SCM_INSTALL_DIR)/dbm; then \
	  $(INSTALL) -d $(SCM_INSTALL_DIR)/dbm; \
	fi
	for f in $(TARGET); do \
	  $(INSTALL) -m 444 $$f $(SCM_INSTALL_DIR)/$$f; \
	done

clean :
	rm -f core *~ wiliki/*~ wiliki/*.orig $(GENERATED)

distclean : clean
	rm -f Makefile

maintainer-clean :
	rm -f Makefile

check :

test :
