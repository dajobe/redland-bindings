# TODO: Take --enable-shared --enable-static into account
all-local: librdf_dlang.a librdf_dlang.so

check: rdf_dlang
	./rdf_dlang

clean-local:
	$(DUB) clean
	rm -f librdf_dlang.a librdf_dlang.so rdf_dlang

lib_LIB = librdf_dlang.a librdf_dlang.so

if DEBUG
buildflags = --build=debug
else
buildflags = --build=release
endif

librdf_dlang.a:
	$(DUB) build --compiler=$(DC) --config=static-library $(buildflags)

librdf_dlang.so:
	$(DUB) build --compiler=$(DC) --config=shared-library $(buildflags)

rdf_dlang:
	$(DUB) build --compiler=$(DC) --config=application --build=unittest

dist-hook:
	cp -a source $(distdir)/
