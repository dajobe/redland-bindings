SUFFIXES: .d
.d.o:
#	$(DC) -od=$(srcdir)/src/$(dir $<) -c $(DFLAGS) -Isrc $<
	$(DC) -of=$< -c $(DFLAGS) -Isrc $<

#lib_LIBRARIES = librdf-dlang.a
AM_DEFAULT_SOURCE_EXT = .d
librdf_dlang_la_LIBADD = @LIBOBJS@
librdf_dlang_la_SOURCES = \
    src/rdf/auxiliary/handled_record.d

lib_LTLIBRARIES = librdf-dlang.la

#librdf_dlang_la_LDFLAGS =
rdf_test_LINK = $(DC)
rdf_test_LDADD = $(D_LDFLAGS) -L=-L.libs -L=-lrdf-dlang
#rdf_test_LDADD = -lphobos
rdf_test_SOURCES = src/rdf_test.d

TESTS = rdf_test
check_PROGRAMS  = rdf_test
