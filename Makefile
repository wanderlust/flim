#
# Makefile for RIME.
#

VERSION = 7.3

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
FLAGS   = -batch -q -no-site-file -l RIME-MK

PREFIX = NONE
LISPDIR = NONE

GOMI	= *.elc
FILES	= README.?? Makefile RIME-MK RIME-CFG RIME-ELS *.el ChangeLog


elc:
	$(EMACS) $(FLAGS) -f compile-rime $(PREFIX) $(LISPDIR)

install:	elc
	$(EMACS) $(FLAGS) -f install-rime $(PREFIX) $(LISPDIR)

clean:
	-$(RM) $(GOMI)
	cd ../tl && make clean


tar:
	cvs commit
	sh -c 'cvs tag -RF rime-`echo $(VERSION) \
				| sed s/\\\\./_/ | sed s/\\\\./_/`; \
	cd /tmp; \
	cvs -d :pserver:anonymous@chamonix.jaist.ac.jp:/hare/cvs/root \
		export -d rime-$(VERSION) \
		-r rime-`echo $(VERSION) | sed s/\\\\./_/ | sed s/\\\\./_/` \
		rime'
	cd /tmp; $(RM) rime-$(VERSION)/ftp.in ; \
		$(TAR) cvzf rime-$(VERSION).tar.gz rime-$(VERSION)
	cd /tmp; $(RM) -r rime-$(VERSION)
	sed "s/VERSION/$(VERSION)/" < ftp.in > ftp
#	-cd ..; mkdir rime-$(VERSION)
#	-$(CP) $(FILES) ../rime-$(VERSION)
#	cd ..; $(TAR) cvzf rime-$(VERSION).tar.gz rime-$(VERSION)
#	cd ..; $(RM) -r rime-$(VERSION)

release:
	-$(RM) /pub/GNU/elisp/apel/rime-$(VERSION).tar.gz
#	cd ..; mv rime-$(VERSION).tar.gz /pub/GNU/elisp/mime/libs/
	mv /tmp/rime-$(VERSION).tar.gz /pub/GNU/elisp/rime/
	cd /pub/GNU/elisp/semi/ ; \
		ln -s ../rime/rime-$(VERSION).tar.gz .
