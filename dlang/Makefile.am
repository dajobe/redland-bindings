all-local: librdf_dlang.a

clean-local:
	$(DUB) clean
	rm -f librdf_dlang.a rdf_dlang

lib_LIB = librdf_dlang.a

librdf_dlang.a:
	$(DUB) build --compiler=$(DC) --config=library
