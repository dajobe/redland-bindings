with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body RDF.Auxiliary.Convert is

   function My_To_C_Without_Nul (Item : in String) return char_array is
   begin
      if Item'Length = 0 then
         return (1..0 => NUL);
      end if;
      --  Append_Nul is False and Item'Length is 0, then To_C propagates Constraint_Error.
      return To_C(Item, Append_Nul=>False);
   end;

   function Value_With_Possible_NULs (Item: RDF.Auxiliary.C_Pointers.Pointer; Length: size_t) return String is
   begin
      return To_Ada(RDF.Auxiliary.C_Pointers.Value(Item, Interfaces.C.ptrdiff_t(Length)), Trim_Nul=>False);
   end;

end RDF.Auxiliary.Convert;
