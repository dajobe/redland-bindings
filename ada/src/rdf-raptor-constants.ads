with Interfaces.C;

package RDF.Raptor.Constants is

   version_major: constant Interfaces.C.unsigned
     with Import, Convention=>C, External_Name=>"raptor_version_major";
   version_minor: constant Interfaces.C.unsigned
     with Import, Convention=>C, External_Name=>"raptor_version_minor";
   version_release: constant Interfaces.C.unsigned
     with Import, Convention=>C, External_Name=>"raptor_version_release";
   version_decimal: constant Interfaces.C.unsigned
     with Import, Convention=>C, External_Name=>"raptor_version_decimal";

   function copyright_string return String;
   function home_url_string return String;
   function license_string return String;
   function short_copyright_string return String;
   function version_string return String;
   function owl_namespace_uri return String;
   function rdf_namespace_uri return String;
   function rdf_schema_namespace_uri return String;
   function xml_literal_datatype_uri_string return String;
   function xml_namespace_uri return String;
   function xmlschema_datatypes_namespace_uri return String;

   Rdf_Namespace_Uri_Len: constant Interfaces.C.unsigned
     with Import, Convention=>C;
   Xml_Literal_Datatype_Uri_String_Len: constant Interfaces.C.unsigned
     with Import, Convention=>C;

end RDF.Raptor.Constants;
