with Interfaces.C.Strings;

package body RDF.Raptor.Constants is

   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;
   function Value(Item : in chars_ptr) return String renames Interfaces.C.Strings.Value;

   Rasqal_copyright_string: constant chars_ptr with Import, Convention=>C;
   Rasqal_home_url_string: constant chars_ptr with Import, Convention=>C;
   Rasqal_license_string: constant chars_ptr with Import, Convention=>C;
   Rasqal_short_copyright_string: constant chars_ptr with Import, Convention=>C;
   Rasqal_version_string: constant chars_ptr with Import, Convention=>C;
   Rasqal_owl_namespace_uri: constant chars_ptr with Import, Convention=>C;
   Rasqal_rdf_namespace_uri: constant chars_ptr with Import, Convention=>C;
   Rasqal_rdf_schema_namespace_uri: constant chars_ptr with Import, Convention=>C;
   Rasqal_xml_literal_datatype_uri_string: constant chars_ptr with Import, Convention=>C;
   Rasqal_xml_namespace_uri: constant chars_ptr with Import, Convention=>C;
   Rasqal_xmlschema_datatypes_namespace_uri: constant chars_ptr with Import, Convention=>C;

   function copyright_string return String is (Value(Rasqal_copyright_string));
   function home_url_string return String is (Value(Rasqal_home_url_string));
   function license_string return String is (Value(Rasqal_license_string));
   function short_copyright_string return String is (Value(Rasqal_short_copyright_string));
   function version_string return String is (Value(Rasqal_version_string));
   function owl_namespace_uri return String is (Value(Rasqal_owl_namespace_uri));
   function rdf_namespace_uri return String is (Value(Rasqal_rdf_namespace_uri));
   function rdf_schema_namespace_uri return String is (Value(Rasqal_rdf_schema_namespace_uri));
   function xml_literal_datatype_uri_string return String is (Value(Rasqal_xml_literal_datatype_uri_string));
   function xml_namespace_uri return String is (Value(Rasqal_xml_namespace_uri));
   function xmlschema_datatypes_namespace_uri return String is (Value(Rasqal_xmlschema_datatypes_namespace_uri));


end RDF.Raptor.Constants;
