with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;
with RDF.Auxiliary;
with RDF.Auxiliary.Convert_Void;

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
      C_Strncpy(New_Str, Str, Len);
      return New_Str;
   end;

   type My_Char_Pointer is access all char with Convention=>C;
   package My_Conv1 is new System.Address_To_Access_Conversions(char);
   package My_Conv2 is new RDF.Auxiliary.Convert_Void(char);
   function My_Conv3 is new Ada.Unchecked_Conversion(chars_ptr, My_Char_Pointer);

   procedure Allocate(Pool : in out Rasqal_Storage_Pool;
                      Storage_Address : out System.Address;
                      Size_In_Storage_Elements : in Storage_Count;
                      Alignment : in Storage_Count) is
   begin
      pragma Assert(RDF.Auxiliary.Dummy_Record'Alignment mod Alignment = 0);
      declare
         Size: constant size_t := size_t(Size_In_Storage_Elements * char'Size / Storage_Unit);
         Ptr: constant My_Char_Pointer := My_Conv3(rasqal_alloc_memory(Size));
      begin
         Storage_Address := My_Conv1.To_Address(My_Conv1.Object_Pointer(Ptr));
      end;
   end;

   overriding procedure Deallocate(Pool : in out Rasqal_Storage_Pool;
                                   Storage_Address : in System.Address;
                                   Size_In_Storage_Elements : in Storage_Count;
                                   Alignment : in Storage_Count) is
   begin
      pragma Assert(RDF.Auxiliary.Dummy_Record'Alignment mod Alignment = 0);
      declare
         Ptr: constant My_Conv2.Object_Pointer :=
           My_Conv2.Object_Pointer(My_Conv1.To_Pointer(Storage_Address));
      begin
         rasqal_free_memory(My_Conv2.To_C_Pointer(Ptr));
      end;
   end;

   overriding function Storage_Size(Pool : Rasqal_Storage_Pool)
                                    return Storage_Count is
   begin
      return RDF.Auxiliary.Dummy_Record_Access'Storage_Size;
   end;

end RDF.Rasqal.Memory;
