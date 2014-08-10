with Interfaces.C.Strings; use Interfaces.C.Strings;

package body RDF.Auxilary is

   use Char_Array_Holders, String_Holders;

   function New_String (Value: String_Holders.Holder) return chars_ptr is
      use String_Holders;
   begin
      return (if Is_Empty(Value) then Null_Ptr else New_String(Element(Value)));
   end;

   function To_C_Object (Item: String_Holders.Holder) return C_String_Result is
   begin
      return Result: C_String_Result do
         if not Is_Empty(Item) then
            Replace_Element(Result, To_C(Element(Item), Append_Nul=>True) );
         end if;
      end return;
   end;

   -- FIXME
--     function Get_C_String (Object: C_String_Result) return chars_ptr is
--     begin
--        if Is_Empty(Object) then
--           return Null_Ptr;
--        else
--           return To_Chars_Ptr(Element(Object)'Access);
--  --           return To_Chars_Ptr(Reference(Object).all'Access);
--        end if;
--     end;

   function Length (Object: C_String_Result) return size_t is
   begin
      if Is_Empty(Object) then
         return 0;
      else
         return Element(Object)'Length;
      end if;
   end;

end RDF.Auxilary;
