package body RDF.Redland.Memory is

   function C_Strncpy (Target, Source: Chars_Ptr; Len: size_t) return chars_ptr
     with Import, Convention=>C, External_Name=>"strncpy";

   procedure C_Strncpy (Target, Source: chars_ptr; Len: size_t) is
      Dummy: chars_ptr := C_Strncpy(Target, Source, Len);
   begin
      null;
   end;

   function Copy_C_String (Str: chars_ptr) return chars_ptr is
      Len: constant size_t := Strlen(Str) + 1;
      New_Str: constant chars_ptr := librdf_alloc_memory(Len);
   begin
      C_Strncpy(New_Str, Str, Len);
      return New_Str;
   end;

end RDF.Redland.Memory;
