with Interfaces.C;

package RDF is

   Raptor_Version_Major: constant Interfaces.C.unsigned
      with Import, Convention=>C, External_Name=>"raptor_version_major";
   Raptor_Version_Minor: constant Interfaces.C.unsigned
      with Import, Convention=>C, External_Name=>"raptor_version_minor";
   Raptor_Version_Release: constant Interfaces.C.unsigned
      with Import, Convention=>C, External_Name=>"raptor_version_release";
   Raptor_Version_Decimal: constant Interfaces.C.unsigned
      with Import, Convention=>C, External_Name=>"raptor_version_decimal";

   function raptor_copyright_string return String;
   function raptor_home_url_string return String;
   function raptor_license_string return String;
   function raptor_short_copyright_string return String;
   function raptor_version_string return String;
   function raptor_owl_namespace_uri return String;
   function raptor_rdf_namespace_uri return String;
   function raptor_rdf_schema_namespace_uri return String;
   function raptor_xml_literal_datatype_uri_string return String;
   function raptor_xml_namespace_uri return String;
   function raptor_xmlschema_datatypes_namespace_uri return String;

end RDF;
