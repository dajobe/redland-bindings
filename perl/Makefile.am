
PERL_PACKAGE=Redland

PERL_MAKEFILE=Makefile.perl

SWIG_OUTPUTS=$(PERL_PACKAGE).pm $(PERL_PACKAGE)_wrap.c $(PERL_PACKAGE)_wrap.doc
SWIG_CRUFT=$(PERL_PACKAGE).bs blib pm_to_blib

EXTRA_DIST=MANIFEST MANIFEST.SKIP Makefile.PL Redland.i test.pl \
$(SWIG_OUTPUTS)

SUBDIRS=lib

CLEANFILES=test*db $(PERL_PACKAGE)_wrap.o $(PERL_MAKEFILE)
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS) $(SWIG_CRUFT)

$(PERL_PACKAGE)_wrap.c: Redland.i
	swig -v -perl5 -package $(PERL_PACKAGE) $?

$(PERL_MAKEFILE): Makefile.PL
	PACKAGE=$(PERL_PACKAGE) VERSION=$(VERSION) TOP_SRCDIR=$(top_srcdir) perl $? PREFIX=$(prefix)

-perl test-perl install-perl clean-perl realclean-perl: $(PERL_MAKEFILE)
	@target=`echo $@ | sed -e 's/-perl//'`; \
	echo $(MAKE) -f $(PERL_MAKEFILE) $$target; \
	$(MAKE) -f $(PERL_MAKEFILE) $$target

build-perl: -perl
install-perl: build-perl
check-perl: build-perl test-perl
