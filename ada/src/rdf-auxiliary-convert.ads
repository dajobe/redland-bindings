with Ada.Unchecked_Conversion;
with Interfaces.C;
with RDF.Auxiliary.C_Pointers;

package RDF.Auxiliary.Convert is

   function Value_With_Possible_NULs (Item: RDF.Auxiliary.C_Pointers.Pointer; Length: size_t) return String;

   function Convert is new Ada.Unchecked_Conversion(RDF.Auxiliary.C_Pointers.Pointer, chars_ptr);
   function Convert is new Ada.Unchecked_Conversion(chars_ptr, RDF.Auxiliary.C_Pointers.Pointer);

end RDF.Auxiliary.Convert;
