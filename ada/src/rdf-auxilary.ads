with Interfaces.C.Strings;

package RDF.Auxilary is

   -- Unused. Remove?
   generic
      C_Ptr: Interfaces.C.Strings.chars_ptr;
   function C_Ptr_To_String return String;

end RDF.Auxilary;
