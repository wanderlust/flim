#
# Makefile for FLIM.
#

PACKAGE = flim
VERSION = 1.9.0

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
FLAGS   = -batch -q -no-site-file -l FLIM-MK

PREFIX = NONE
LISPDIR = NONE

GOMI	= *.elc
FILES	= README.?? Makefile FLIM-MK FLIM-CFG FLIM-ELS *.el ChangeLog


elc: ew-parse.el
	$(EMACS) $(FLAGS) -f compile-flim $(PREFIX) $(LISPDIR)

install:	elc
	$(EMACS) $(FLAGS) -f install-flim $(PREFIX) $(LISPDIR)

clean:
	-$(RM) $(GOMI)


tar:
	cvs commit
	sh -c 'cvs tag -RF $(PACKAGE)-`echo $(VERSION) \
				| sed s/\\\\./_/ | sed s/\\\\./_/`; \
	cd /tmp; \
	cvs -d :pserver:anonymous@chamonix.jaist.ac.jp:/hare/cvs/root \
		export -d $(PACKAGE)-$(VERSION) \
		-r $(PACKAGE)-`echo $(VERSION) | sed s/\\\\./_/ | sed s/\\\\./_/` \
		flim'
	cd /tmp; $(RM) $(PACKAGE)-$(VERSION)/ftp.in ; \
		$(TAR) cvzf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)
	cd /tmp; $(RM) -r $(PACKAGE)-$(VERSION)
	sed "s/VERSION/$(VERSION)/" < ftp.in > ftp

release:
	-$(RM) /pub/GNU/elisp/apel/$(PACKAGE)-$(VERSION).tar.gz
	mv /tmp/$(PACKAGE)-$(VERSION).tar.gz /pub/GNU/elisp/flim/
	cd /pub/GNU/elisp/semi/ ; \
		ln -s ../flim/$(PACKAGE)-$(VERSION).tar.gz .

ew-parse.el: ew-parse.scm lalr-el.scm
	-scm -f lalr-el.scm -f ew-parse.scm > ew-parse.out

check:
	$(EMACS) -q -batch -eval '(setq load-path (cons "." load-path))' -l ./TESTPAT -eval '(report)'


# BENCHMARK is not a part of FLAM-DOODLE because it is so large.
benchmark:
	$(EMACS) -q -batch -eval '(setq load-path (cons "." load-path))' -l ./BENCHMARK -eval '(report)'

