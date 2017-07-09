with Interfaces.C.Strings;

package body RDF.Raptor.Constants is

   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;
   function Value(Item : in chars_ptr) return String renames Interfaces.C.Strings.Value;

   raptor_copyright_string: constant chars_ptr with Import, Convention=>C;
   raptor_home_url_string: constant chars_ptr with Import, Convention=>C;
   raptor_license_string: constant chars_ptr with Import, Convention=>C;
   raptor_short_copyright_string: constant chars_ptr with Import, Convention=>C;
   raptor_version_string: constant chars_ptr with Import, Convention=>C;
   raptor_owl_namespace_uri: constant chars_ptr with Import, Convention=>C;
   raptor_rdf_namespace_uri: constant chars_ptr with Import, Convention=>C;
   raptor_rdf_schema_namespace_uri: constant chars_ptr with Import, Convention=>C;
   raptor_xml_literal_datatype_uri_string: constant chars_ptr with Import, Convention=>C;
   raptor_xml_namespace_uri: constant chars_ptr with Import, Convention=>C;
   raptor_xmlschema_datatypes_namespace_uri: constant chars_ptr with Import, Convention=>C;

   function copyright_string return String is (Value(raptor_copyright_string));
   function home_url_string return String is (Value(raptor_home_url_string));
   function license_string return String is (Value(raptor_license_string));
   function short_copyright_string return String is (Value(raptor_short_copyright_string));
   function version_string return String is (Value(raptor_version_string));
   function owl_namespace_uri return String is (Value(raptor_owl_namespace_uri));
   function rdf_namespace_uri return String is (Value(raptor_rdf_namespace_uri));
   function rdf_schema_namespace_uri return String is (Value(raptor_rdf_schema_namespace_uri));
   function xml_literal_datatype_uri_string return String is (Value(raptor_xml_literal_datatype_uri_string));
   function xml_namespace_uri return String is (Value(raptor_xml_namespace_uri));
   function xmlschema_datatypes_namespace_uri return String is (Value(raptor_xmlschema_datatypes_namespace_uri));


end RDF.Raptor.Constants;
