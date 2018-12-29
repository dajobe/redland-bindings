SUFFIXES: .d
.d.o:
	$(DC) -od=$(srcdir)/src -c $(DFLAGS) $<

#lib_LIBRARIES = librdf-dlang.a

librdf_dlang_la_LIBADD = @LIBOBJS@
librdf_dlang_la_SOURCES = \
    src/rdf.d

lib_LTLIBRARIES = librdf-dlang.la

#librdf_dlang_la_LDFLAGS =
rdf_test_LINK = $(DC)
#rdf_test_LDADD = $(D_LDFLAGS) -lrdf-dlang
#rdf_test_LDADD = -lphobos
rdf_test_SOURCES  = src/rdf_test.d

TESTS = rdf_test
check_PROGRAMS  = rdf_test
