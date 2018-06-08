EXTRA_DIST = ada-bindings.odt ada-bindings.html ada-bindings.pdf

ada-bindings.html: ada-bindings.odt
	libreoffice --headless --convert-to "html:XHTML Writer File:UTF8" $<

ada-bindings.pdf: ada-bindings.odt
	libreoffice --headless --convert-to pdf $<

all-local: ada-bindings.html ada-bindings.pdf
