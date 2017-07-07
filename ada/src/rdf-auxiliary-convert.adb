with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body RDF.Auxiliary.Convert is

   function Value_With_Possible_NULs (Item: RDF.Auxiliary.C_Pointers.Pointer; Length: size_t) return String is
   begin
      return To_Ada(RDF.Auxiliary.C_Pointers.Value(Item, Interfaces.C.ptrdiff_t(Length)), Trim_Nul=>False);
   end;

end RDF.Auxiliary.Convert;
