#
# Makefile for FLIM.
#

VERSION = 1.3.0

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
FLAGS   = -batch -q -no-site-file -l FLIM-MK

PREFIX = NONE
LISPDIR = NONE

GOMI	= *.elc
FILES	= README.?? Makefile FLIM-MK FLIM-CFG FLIM-ELS *.el ChangeLog


elc:
	$(EMACS) $(FLAGS) -f compile-flim $(PREFIX) $(LISPDIR)

install:	elc
	$(EMACS) $(FLAGS) -f install-flim $(PREFIX) $(LISPDIR)

clean:
	-$(RM) $(GOMI)
	cd ../tl && make clean


tar:
	cvs commit
	sh -c 'cvs tag -RF flim-`echo $(VERSION) \
				| sed s/\\\\./_/ | sed s/\\\\./_/`; \
	cd /tmp; \
	cvs -d :pserver:anonymous@chamonix.jaist.ac.jp:/hare/cvs/root \
		export -d flim-$(VERSION) \
		-r flim-`echo $(VERSION) | sed s/\\\\./_/ | sed s/\\\\./_/` \
		flim'
	cd /tmp; $(RM) flim-$(VERSION)/ftp.in ; \
		$(TAR) cvzf flim-$(VERSION).tar.gz flim-$(VERSION)
	cd /tmp; $(RM) -r flim-$(VERSION)
	sed "s/VERSION/$(VERSION)/" < ftp.in > ftp

release:
	-$(RM) /pub/GNU/elisp/apel/flim-$(VERSION).tar.gz
	mv /tmp/flim-$(VERSION).tar.gz /pub/GNU/elisp/flim/
	cd /pub/GNU/elisp/semi/ ; \
		ln -s ../flim/flim-$(VERSION).tar.gz .
