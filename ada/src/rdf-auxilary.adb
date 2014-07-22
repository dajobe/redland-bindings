with Interfaces.C.Strings;

package body RDF.Auxilary is

   function C_Ptr_To_String return String is
   begin
      return Interfaces.C.Strings.Value(C_Ptr);
   end;

end RDF.Auxilary;
