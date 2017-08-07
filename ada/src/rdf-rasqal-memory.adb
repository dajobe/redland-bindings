package body RDF.Rasqal.Memory is

   function C_Strncpy (Target, Source: Chars_Ptr; Len: size_t) return chars_ptr
      with Import, Convention=>C, External_Name=>"strncpy";

   procedure C_Strncpy (Target, Source: chars_ptr; Len: size_t) is
      Dummy: chars_ptr := C_Strncpy(Target, Source, Len);
   begin
      null;
   end;

   function Copy_C_String (Str: chars_ptr) return Chars_Ptr is
      Len: constant size_t := Strlen(Str) + 1;
      New_Str: constant Chars_Ptr := rasqal_alloc_memory(Len);
   begin
      C_Strncpy(New_Str, Str, size_t(Len));
      return New_Str;
   end;

end RDF.Rasqal.Memory;
