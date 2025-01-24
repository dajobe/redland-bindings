with Interfaces.C.Strings;

package body RDF.Rasqal.Constants is

   subtype chars_ptr is Interfaces.C.Strings.chars_ptr;
   function Value(Item : in chars_ptr) return String renames Interfaces.C.Strings.Value;

   rasqal_copyright_string: constant chars_ptr with Import, Convention=>C;
   rasqal_home_url_string: constant chars_ptr with Import, Convention=>C;
   rasqal_license_string: constant chars_ptr with Import, Convention=>C;
   rasqal_short_copyright_string: constant chars_ptr with Import, Convention=>C;
   rasqal_version_string: constant chars_ptr with Import, Convention=>C;

   function copyright_string return String is (Value(rasqal_copyright_string));
   function home_url_string return String is (Value(rasqal_home_url_string));
   function license_string return String is (Value(rasqal_license_string));
   function short_copyright_string return String is (Value(rasqal_short_copyright_string));
   function version_string return String is (Value(rasqal_version_string));

end RDF.Rasqal.Constants;
