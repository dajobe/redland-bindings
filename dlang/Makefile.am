all-local: librdf_dlang.a

check-local: rdf_dlang
	./rdf_dlang

clean-local:
	$(DUB) clean
	rm -f librdf_dlang.a rdf_dlang

lib_LIB = librdf_dlang.a

if DEBUG
buildflags = --build=debug
else
buildflags = --build=release
endif

librdf_dlang.a:
	$(DUB) build --compiler=$(DC) --config=library $(buildflags)

rdf_dlang:
	$(DUB) build --compiler=$(DC) --config=application --build=unittest

dist-hook:
	cp -a source $(distdir)/
