with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.World; use RDF.Raptor.World;

package RDF.Raptor.Options is

   -- Limit to these options which are explicitly specified below.
   -- If you try to do with an other option, it can lead to Constraint_Error in Ada :-(
   -- Thus we need maintain updating this list, when the corresponding enum in C changes.
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
   -- Use the default representation (it may probably be faster)
   --     for Raptor_Option use (SCANNING => 0,
   --                            ALLOW_NON_NS_ATTRIBUTES => 1,
   --                            ALLOW_OTHER_PARSETYPES => 2,
   --                            ALLOW_BAGID => 3,
   --                            ALLOW_RDF_TYPE_RDF_LIST => 4,
   --                            NORMALIZE_LANGUAGE => 5,
   --                            NON_NFC_FATAL => 6,
   --                            WARN_OTHER_PARSETYPES => 7,
   --                            CHECK_RDF_ID => 8,
   --                            RELATIVE_URIS => 9,
   --                            WRITER_AUTO_INDENT => 10,
   --                            WRITER_AUTO_EMPTY => 11,
   --                            WRITER_INDENT_WIDTH => 12,
   --                            WRITER_XML_VERSION => 13,
   --                            WRITER_XML_DECLARATION => 14,
   --                            NO_NET => 15,
   --                            RESOURCE_BORDER => 16,
   --                            LITERAL_BORDER => 17,
   --                            BNODE_BORDER => 18,
   --                            RESOURCE_FILL => 19,
   --                            LITERAL_FILL => 20,
   --                            BNODE_FILL => 21,
   --                            HTML_TAG_SOUP => 22,
   --                            MICROFORMATS => 23,
   --                            HTML_LINK => 24,
   --                            WWW_TIMEOUT => 25,
   --                            WRITE_BASE_URI => 26,
   --                            WWW_HTTP_CACHE_CONTROL => 27,
   --                            WWW_HTTP_USER_AGENT => 28,
   --                            JSON_CALLBACK => 29,
   --                            JSON_EXTRA_DATA => 30,
   --                            RSS_TRIPLES => 31,
   --                            ATOM_ENTRY_URI => 32,
   --                            PREFIX_ELEMENTS => 33,
   --                            STRICT => 34,
   --                            WWW_CERT_FILENAME => 35,
   --                            WWW_CERT_TYPE => 36,
   --                            WWW_CERT_PASSPHRASE => 37,
   --                            NO_FILE => 38,
   --                            WWW_SSL_VERIFY_PEER => 39,
   --                            WWW_SSL_VERIFY_HOST => 40,
   --                            LOAD_EXTERNAL_ENTITIES => 41);

   function Last return Raptor_Option renames LOAD_EXTERNAL_ENTITIES;

   type Value_Type_Type is (TYPE_BOOL, TYPE_INT, TYPE_STRING, TYPE_URI)
     with Convention => C;

   function Last return Value_Type_Type renames TYPE_URI;

   type Option_Description_Record is private;

   type Option_Description_Record_Access is access Option_Description_Record
     with Convention => C;

   package Option_Description_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(Option_Description_Record, Option_Description_Record_Access);

   type Option_Description_Type_Without_Finalize is new Option_Description_Handled_Record.Base_Object with null record;

   subtype Option_Description_Handle is Option_Description_Handled_Record.Access_Type;

   function Get_Domain (Description: Option_Description_Type_Without_Finalize) return Domain_Type;

   function Get_Option (Description: Option_Description_Type_Without_Finalize) return Raptor_Option;

   function Get_Value_Type (Description: Option_Description_Type_Without_Finalize) return Value_Type_Type;

   function Get_Name (Description: Option_Description_Type_Without_Finalize) return String;

   function Get_Label (Description: Option_Description_Type_Without_Finalize) return String;

   function Get_URI (Description: Option_Description_Type_Without_Finalize) return URI_Type_Without_Finalize;

   -- WARNING: Raptor_Option'Val(Get_Options_Count - 1) may throw Constraint_Error
   function Get_Options_Count return Natural;

   function Value_Type_Label (Value_Type: Value_Type_Type) return String;

   package Finalizer is new With_Finalization(Option_Description_Type_Without_Finalize);

   type Option_Description_Type is new Finalizer.Derived with null record;

   overriding procedure Finalize_Handle (Object: Option_Description_Type; Handle: Option_Description_Handle);

   not overriding function Get_Option_Description (World: Raptor_World_Type_Without_Finalize'Class;
                                                   Domain: Domain_Type;
                                                   Option: Natural)
                                                   return Option_Description_Type;

   not overriding function Get_Option_Description (World: Raptor_World_Type_Without_Finalize'Class;
                                                   Domain: Domain_Type;
                                                   Option: Raptor_Option)
                                                   return Option_Description_Type;

   function Option_From_URI (World: Raptor_World_Type_Without_Finalize'Class; URI: URI_Type'Class)
                             return Raptor_Option;

private

   type Option_Description_Record is
      record
         Domain: Domain_Type;
         Option: Raptor_Option;
         Value_Type: Value_Type_Type;
         Name: chars_ptr;
         Len: size_t;
         Label: chars_ptr;
         URI: URI_Handle;
      end record
     with Convention => C;

end RDF.Raptor.Options;
