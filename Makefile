#
# Makefile for FLIM.
#

PACKAGE = flim
API	= 1.12
RELEASE = 5

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -eval "$${EVALARGS:-nil}"
FLAGS_CURDIR   = $(FLAGS) -eval '(setq load-path (cons "." load-path))'

PREFIX = NONE
LISPDIR = NONE
PACKAGEDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE

GOMI	= *.elc \
	  *.cp *.cps *.ky *.kys *.fn *.fns *.vr *.vrs \
	  *.pg *.pgs *.tp *.tps *.toc *.aux *.log
FILES	= README.?? Makefile FLIM-MK FLIM-CFG FLIM-ELS *.el ChangeLog

VERSION	= $(API).$(RELEASE)
ARC_DIR = /pub/GNU/elisp/flim/$(PACKAGE)-$(API)
SEMI_ARC_DIR = /pub/GNU/elisp/semi/semi-1.13-for-flim-$(API)

elc: ew-parse.el
	$(EMACS) $(FLAGS) -l FLIM-MK -f compile-flim $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)

install:	elc
	$(EMACS) $(FLAGS) -l FLIM-MK -f install-flim $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)


package:
	$(XEMACS) $(FLAGS) -f compile-flim-package $(PACKAGEDIR)

install-package:	package
	$(XEMACS) $(FLAGS) -f install-flim-package $(PACKAGEDIR)

clean:
	-$(RM) $(GOMI)


tar:
	cvs commit
	sh -c 'cvs tag -RF $(PACKAGE)-`echo $(VERSION) | tr . _`; \
	cd /tmp; \
	cvs -d :pserver:anonymous@chamonix.jaist.ac.jp:/hare/cvs/root \
		export -d $(PACKAGE)-$(VERSION) \
		-r $(PACKAGE)-`echo $(VERSION) | tr . _` \
		flim'
	cd /tmp; $(RM) $(PACKAGE)-$(VERSION)/ftp.in ; \
		$(TAR) cvzf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)
	cd /tmp; $(RM) -r $(PACKAGE)-$(VERSION)
	sed "s/VERSION/$(VERSION)/" < ftp.in | sed "s/API/$(API)/" > ftp

release:
	-$(RM) $(ARC_DIR)/$(PACKAGE)-$(VERSION).tar.gz
	mv /tmp/$(PACKAGE)-$(VERSION).tar.gz $(ARC_DIR)
	cd $(SEMI_ARC_DIR) ; \
		ln -s ../../flim/flim-$(API)/$(PACKAGE)-$(VERSION).tar.gz .

ew-parse.el: ew-parse.scm lalr-el.scm
	-scm -f lalr-el.scm -f ew-parse.scm > ew-parse.out

check:
	$(EMACS) -batch -q -no-site-file -l TESTPAT -l TESTPAT-DOODLE -f report-doodle 2>&1

check-verbose:
	$(EMACS) -batch -q -no-site-file -l TESTPAT -l TESTPAT-DOODLE -f report-doodle-verbose 2>&1

# The file BENCHMARK is not a part of FLAM-DOODLE because it is so large.
benchmark:
	$(EMACS) $(FLAGS_CURDIR) -l ./BENCHMARK -eval '(report)'
