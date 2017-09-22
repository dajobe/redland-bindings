with Interfaces.C;

package RDF.Rasqal.Constants is

   version_major: constant Interfaces.C.unsigned
      with Import, Convention=>C, External_Name=>"rasqal_version_major";
   version_minor: constant Interfaces.C.unsigned
      with Import, Convention=>C, External_Name=>"rasqal_version_minor";
   version_release: constant Interfaces.C.unsigned
      with Import, Convention=>C, External_Name=>"rasqal_version_release";
   version_decimal: constant Interfaces.C.unsigned
      with Import, Convention=>C, External_Name=>"rasqal_version_decimal";

   function copyright_string return String;
   function home_url_string return String;
   function license_string return String;
   function short_copyright_string return String;
   function version_string return String;

end RDF.Rasqal.Constants;
