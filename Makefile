#
# Makefile for FLIM.
#

PACKAGE = flim-chao
API	= 1.14
RELEASE = 1

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l FLIM-MK

PREFIX = NONE
LISPDIR = NONE
PACKAGEDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE

GOMI	= *.elc \
	  *.cp *.cps *.ky *.kys *.fn *.fns *.vr *.vrs \
	  *.pg *.pgs *.tp *.tps *.toc *.aux *.log
FILES	= README.?? Makefile FLIM-MK FLIM-CFG FLIM-ELS *.el ChangeLog

VERSION	= $(API).$(RELEASE)
ARC_DIR_PREFIX = /home/tomo/public_html/comp/emacsen/lisp
ARC_DIR = $(ARC_DIR_PREFIX)/flim/flim-$(API)
SEMI_ARC_DIR = $(ARC_DIR_PREFIX)/semi/semi-1.14-for-flim-$(API)

elc:
	$(EMACS) $(FLAGS) -f compile-flim $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)

install:	elc
	$(EMACS) $(FLAGS) -f install-flim $(PREFIX) $(LISPDIR) \
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
	cvs -d :pserver:anonymous@cvs.m17n.org:/cvs/root \
		export -d $(PACKAGE)-$(VERSION) \
		-r $(PACKAGE)-`echo $(VERSION) | tr . _` \
		flim'
	cd /tmp; $(RM) $(PACKAGE)-$(VERSION)/ftp.in ; \
		$(TAR) cvzf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)
	cd /tmp; $(RM) -r $(PACKAGE)-$(VERSION)
	sed "s/VERSION/$(VERSION)/" < ftp.in | sed "s/API/$(API)/" \
		| sed "s/PACKAGE/$(PACKAGE)/" > ftp

release:
	-$(RM) $(ARC_DIR)/$(PACKAGE)-$(VERSION).tar.gz
	mv /tmp/$(PACKAGE)-$(VERSION).tar.gz $(ARC_DIR)
	cd $(SEMI_ARC_DIR) ; \
		ln -s ../../flim/flim-$(API)/$(PACKAGE)-$(VERSION).tar.gz .
