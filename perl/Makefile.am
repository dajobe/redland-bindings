
PERL_PACKAGE=Redland
SWIG_OUTPUTS=$(PERL_PACKAGE).pm $(PERL_PACKAGE)_wrap.c $(PERL_PACKAGE)_wrap.doc

EXTRA_DIST=MANIFEST MANIFEST.SKIP Makefile.PL Redland.i test.pl \
$(SWIG_OUTPUTS)

SUBDIRS=lib

CLEANFILES=test*db
MAINTAINERCLEANFILES=$(SWIG_OUTPUTS)

$(PERL_PACKAGE)_wrap.c: Redland.i
	swig -v -perl5 -package $(PERL_PACKAGE) Redland.i

Makefile.perl: Makefile.PL
	perl Makefile.PL
