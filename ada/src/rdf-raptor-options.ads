with Interfaces.C; use Interfaces.C;
with RDF.Auxilary.Limited_Handled_Record;

package RDF.Raptor.Options is

   type Raptor_Option is (SCANNING,
                          ALLOW_NON_NS_ATTRIBUTES,
                          ALLOW_OTHER_PARSETYPES,
                          ALLOW_BAGID,
                          ALLOW_RDF_TYPE_RDF_LIST,
                          NORMALIZE_LANGUAGE,
                          NON_NFC_FATAL,
                          WARN_OTHER_PARSETYPES,
                          CHECK_RDF_ID,
                          RELATIVE_URIS,
                          WRITER_AUTO_INDENT,
                          WRITER_AUTO_EMPTY,
                          WRITER_INDENT_WIDTH,
                          WRITER_XML_VERSION,
                          WRITER_XML_DECLARATION,
                          NO_NET,
                          RESOURCE_BORDER,
                          LITERAL_BORDER,
                          BNODE_BORDER,
                          RESOURCE_FILL,
                          LITERAL_FILL,
                          BNODE_FILL,
                          HTML_TAG_SOUP,
                          MICROFORMATS,
                          HTML_LINK,
                          WWW_TIMEOUT,
                          WRITE_BASE_URI,
                          WWW_HTTP_CACHE_CONTROL,
                          WWW_HTTP_USER_AGENT,
                          JSON_CALLBACK,
                          JSON_EXTRA_DATA,
                          RSS_TRIPLES,
                          ATOM_ENTRY_URI,
                          PREFIX_ELEMENTS,
                          STRICT,
                          WWW_CERT_FILENAME,
                          WWW_CERT_TYPE,
                          WWW_CERT_PASSPHRASE,
                          NO_FILE,
                          WWW_SSL_VERIFY_PEER,
                          WWW_SSL_VERIFY_HOST,
                          LOAD_EXTERNAL_ENTITIES)
      with Convention => C;
   for Raptor_Option use (SCANNING => 0,
                          ALLOW_NON_NS_ATTRIBUTES => 1,
                          ALLOW_OTHER_PARSETYPES => 2,
                          ALLOW_BAGID => 3,
                          ALLOW_RDF_TYPE_RDF_LIST => 4,
                          NORMALIZE_LANGUAGE => 5,
                          NON_NFC_FATAL => 6,
                          WARN_OTHER_PARSETYPES => 7,
                          CHECK_RDF_ID => 8,
                          RELATIVE_URIS => 9,
                          WRITER_AUTO_INDENT => 10,
                          WRITER_AUTO_EMPTY => 11,
                          WRITER_INDENT_WIDTH => 12,
                          WRITER_XML_VERSION => 13,
                          WRITER_XML_DECLARATION => 14,
                          NO_NET => 15,
                          RESOURCE_BORDER => 16,
                          LITERAL_BORDER => 17,
                          BNODE_BORDER => 18,
                          RESOURCE_FILL => 19,
                          LITERAL_FILL => 20,
                          BNODE_FILL => 21,
                          HTML_TAG_SOUP => 22,
                          MICROFORMATS => 23,
                          HTML_LINK => 24,
                          WWW_TIMEOUT => 25,
                          WRITE_BASE_URI => 26,
                          WWW_HTTP_CACHE_CONTROL => 27,
                          WWW_HTTP_USER_AGENT => 28,
                          JSON_CALLBACK => 29,
                          JSON_EXTRA_DATA => 30,
                          RSS_TRIPLES => 31,
                          ATOM_ENTRY_URI => 32,
                          PREFIX_ELEMENTS => 33,
                          STRICT => 34,
                          WWW_CERT_FILENAME => 35,
                          WWW_CERT_TYPE => 36,
                          WWW_CERT_PASSPHRASE => 37,
                          NO_FILE => 38,
                          WWW_SSL_VERIFY_PEER => 39,
                          WWW_SSL_VERIFY_HOST => 40,
                          LOAD_EXTERNAL_ENTITIES => 41);

   function Last return Raptor_Option renames LOAD_EXTERNAL_ENTITIES;

   type Option_Description_Type is private;

   package Handled_Record is new RDF.Auxilary.Limited_Handled_Record(Option_Description_Type);

   type Option_Description_Type_Without_Finalize is new Handled_Record.Base_Object with null record;

   subtype Handle_Type is Handled_Record.Access_Type;

private

   type Option_Description_Type is
      record
         Domain: Domain_Type;
         -- TODO
      end record
      with Convention => C;

end RDF.Raptor.Options;
