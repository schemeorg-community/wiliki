
.PHONY: test clean distclean dist install

GOSH = gosh
GOSH_EUC = gosh-euc
GAUCHE_CONFIG = gauche-config
INSTALL = /usr/bin/install -c
INSTALL_TYPE = site
SCM_INSTALL_DIR = $(DESTDIR)`$(GAUCHE_CONFIG) --$(INSTALL_TYPE)libdir`
SCM_PATCH_DIR = $(DESTDIR)`$(GAUCHE_CONFIG) --syslibdir`
UNICODEURL = "http://www.unicode.org/Public/UNIDATA"
VERSION=`cat VERSION`
TESTENV=

# data from unicode.org not included, so nothing builds by default
#all: case-maps char-sets
all:

install: all
	for f in `cd lib && find . -name \*.scm`; do      \
	  $(INSTALL) -D -m 644 lib/$$f $(SCM_INSTALL_DIR)/$$f; \
	done
	cp data/case-map-*.dat $(SCM_INSTALL_DIR)

srfi-14-patch:
	cp srfi-14.scm.diff $(SCM_PATCH_DIR)
	cd $(SCM_PATCH_DIR) && patch -p 0 < srfi-14.scm.diff && rm -f srfi-14.scm.diff
	$(INSTALL) -D -m 644 data/ranges.dat $(SCM_PATCH_DIR)/srfi-14/ranges.dat

test: all
	cd test;                                \
	for f in t-*.scm; do                    \
	  $(TESTENV) $(GOSH) $$f;               \
	done

clean:
	rm -f `find . -name \*~`

distclean: clean
	rm -rf debian/tmp

# build from Unicode data (not included)

ranges:
	if type $(GOSH_EUC) 2>/dev/null; then               \
	  $(GOSH_EUC) ./tools/gen-ranges >data/ranges.dat;  \
	else                                                \
	  $(GOSH) ./tools/gen-ranges >data/ranges.dat;      \
	fi

case-maps: case-map-1 case-map-2

case-map-1: UnicodeData.txt
	./tools/extract-case-1.scm < $< > data/case-map-1.dat

case-map-2: SpecialCasing.txt
	./tools/extract-case-2.scm < $< > data/case-map-2.dat

# chained autoload is causing problems
#char-sets: Scripts.txt PropList.txt DerivedCoreProperties.txt
#	./tools/extract-prop.scm -g -m lang.char-set.other -o lib/lang/char-set/other.scm -i "other" PropList.txt
#	./tools/extract-prop.scm -g -m lang.char-set.derived -o lib/lang/char-set/derived.scm -f PropList.txt -A lib/lang/char-set/other.scm DerivedCoreProperties.txt
#	./tools/extract-prop.scm -g -m lang.char-set -o lib/lang/char-set.scm -A lib/lang/char-set/other.scm,lib/lang/char-set/derived.scm Scripts.txt
#	./tools/extract-prop.scm -g -a lib/lang/char-set.scm -x "other" PropList.txt

char-sets: Scripts.txt PropList.txt DerivedCoreProperties.txt
	./tools/extract-prop.scm -g -m lang.char-set -e -d common -o lib/lang/char-set.scm Scripts.txt
	./tools/extract-prop.scm -g -a lib/lang/char-set.scm PropList.txt
	./tools/extract-prop.scm -g -f PropList.txt -a lib/lang/char-set.scm DerivedCoreProperties.txt

UnicodeData.txt SpecialCasing.txt Scripts.txt PropList.txt DerivedCoreProperties.txt:
	wget $(UNICODEURL)/$@

# build distribution tarballs
dist: all distclean
	dir=`basename $$PWD`             && \
	name=$${dir}-$(VERSION)          && \
	cd ..                            && \
	tar cjvf $$name.tar.bz2 -h --exclude \*.txt $$dir && \
	cd $$dir

