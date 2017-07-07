with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Deallocation;

package body RDF.Auxiliary is

   function Length (Object: String_Holders.Holder) return size_t is
      use String_Holders;
   begin
      if Is_Empty(Object) then
         return 0;
      else
         return Element(Object)'Length;
      end if;
   end;

   function Sign (Value: Interfaces.C.int) return Comparison_Result is
   begin
      if Value < 0 then
         return -1;
      else
         return (if Value > 0 then 1 else 0);
      end if;
   end;

end RDF.Auxiliary;
