with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Deallocation;

package body RDF.Auxilary is

   function Length (Object: String_Holders.Holder) return size_t is
      use String_Holders;
   begin
      if Is_Empty(Object) then
         return 0;
      else
         return Element(Object)'Length;
      end if;
   end;

end RDF.Auxilary;
