with Interfaces.C.Strings;
with RDF.Auxilary;

package body RDF is

   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;
   function Value(Item : in chars_ptr) return String renames Interfaces.C.Strings.Value;

   c_raptor_copyright_string: constant chars_ptr with Import, Convention=>C, External_Name=>"raptor_copyright_string";
   c_raptor_home_url_string: constant chars_ptr with Import, Convention=>C, External_Name=>"raptor_home_url_string";
   c_raptor_license_string: constant chars_ptr with Import, Convention=>C, External_Name=>"raptor_license_string";
   c_raptor_short_copyright_string: constant chars_ptr with Import, Convention=>C, External_Name=>"raptor_short_copyright_string";
   c_raptor_version_string: constant chars_ptr with Import, Convention=>C, External_Name=>"raptor_version_string";
   c_raptor_owl_namespace_uri: constant chars_ptr with Import, Convention=>C, External_Name=>"raptor_owl_namespace_uri";
   c_raptor_rdf_namespace_uri: constant chars_ptr with Import, Convention=>C, External_Name=>"raptor_rdf_namespace_uri";
   c_raptor_rdf_schema_namespace_uri: constant chars_ptr with Import, Convention=>C, External_Name=>"raptor_rdf_schema_namespace_uri";
   c_raptor_xml_literal_datatype_uri_string: constant chars_ptr with Import, Convention=>C, External_Name=>"raptor_xml_literal_datatype_uri_string";
   c_raptor_xml_namespace_uri: constant chars_ptr with Import, Convention=>C, External_Name=>"raptor_xml_namespace_uri";
   c_raptor_xmlschema_datatypes_namespace_uri: constant chars_ptr with Import, Convention=>C, External_Name=>"raptor_xmlschema_datatypes_namespace_uri";

   function raptor_copyright_string return String is (Value(c_raptor_copyright_string));
   function raptor_home_url_string return String is (Value(c_raptor_home_url_string));
   function raptor_license_string return String is (Value(c_raptor_license_string));
   function raptor_short_copyright_string return String is (Value(c_raptor_short_copyright_string));
   function raptor_version_string return String is (Value(c_raptor_version_string));
   function raptor_owl_namespace_uri return String is (Value(c_raptor_owl_namespace_uri));
   function raptor_rdf_namespace_uri return String is (Value(c_raptor_rdf_namespace_uri));
   function raptor_rdf_schema_namespace_uri return String is (Value(c_raptor_rdf_schema_namespace_uri));
   function raptor_xml_literal_datatype_uri_string return String is (Value(c_raptor_xml_literal_datatype_uri_string));
   function raptor_xml_namespace_uri return String is (Value(c_raptor_xml_namespace_uri));
   function raptor_xmlschema_datatypes_namespace_uri return String is (Value(c_raptor_xmlschema_datatypes_namespace_uri));

end RDF;
