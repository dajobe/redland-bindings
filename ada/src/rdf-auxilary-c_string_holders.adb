package body RDF.Auxilary.C_String_Holders is

   use String_Holders, Char_Array_Holders;

   function Length (Object: C_String_Holder) return size_t is
   begin
      if Is_Empty(Object) then
         return 0;
      else
         return Element(Object)'Length;
      end if;
   end;

   function New_String (Value: String_Holders.Holder) return chars_ptr is
      use String_Holders;
   begin
      return (if Is_Empty(Value) then Null_Ptr else New_String(Element(Value)));
   end;

   function To_C_String_Holder (Item: String_Holders.Holder) return C_String_Holder is
   begin
      return Result: C_String_Holder do
         if not Is_Empty(Item) then
            Replace_Element(Result, To_C(Element(Item), Append_Nul=>True) );
         end if;
      end return;
   end;

   function C_String (Object: C_String_Holder) return chars_ptr is
   begin
      if Is_Empty(Object) then
         return Null_Ptr;
      else
         return To_Chars_Ptr(Char_Array_Access(Element_Access(Object)));
--           return To_Chars_Ptr(Reference(Object).all'Access);
      end if;
   end;

end RDF.Auxilary.C_String_Holders;
