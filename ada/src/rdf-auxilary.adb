with Interfaces.C.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body RDF.Auxilary is

   function Null_Value return String_Or_Null is (Has_Value=>False);

   function From_String (Str: String) return String_Or_Null is
        (Has_Value=>True, Value=>To_Unbounded_String(Str));

   function To_String (Value: String_Or_Null) return String is (To_String(Value.Value));

   function Is_Null (Value: String_Or_Null) return Boolean is (not Value.Has_Value);

   function New_String (Value: String_Or_Null) return chars_ptr is
   begin
      return (if Value.Has_Value then New_String(To_String(Value.Value)) else Null_Ptr);
   end;

--     function To_C (Value: String_Or_Null; Data: aliased out char_array) return chars_ptr is
--     begin
--        if Value.Has_Value then
--           Data := To_C(To_String(Value.Value), Append_Nul=>True);
--  	 return To_Chars_Ptr(Data'Unchecked_Access, Nul_Check=>False);
--        else
--           return Null_Ptr;
--        end if;
--     end;

end RDF.Auxilary;
