with Ada.Unchecked_Conversion;

package body RDF.Auxiliary.Convert_Void is

   type My_Char_Pointer is access all char with Convention=>C;

   function From_My_Char_Pointer is new Ada.Unchecked_Conversion(My_Char_Pointer, chars_ptr);
   function From_Chars_Ptr is new Ada.Unchecked_Conversion(chars_ptr, My_Char_Pointer);

   package Char_Address_Conversions is new System.Address_To_Access_Conversions(char);

   function To_Access (Void_Pointer: chars_ptr) return Object_Pointer is
      P: constant My_Char_Pointer := From_Chars_Ptr(Void_Pointer);
      A: constant System.Address :=
        Char_Address_Conversions.To_Address(Char_Address_Conversions.Object_Pointer(P));
   begin
      return Address_Conversions.To_Pointer(A);
   end;

   function To_C_Pointer (Pointer: Object_Pointer) return chars_ptr is
      A: constant System.Address := Address_Conversions.To_Address(Pointer);
      P: constant My_Char_Pointer :=
        My_Char_Pointer(Char_Address_Conversions.To_Pointer(A));
   begin
      return From_My_Char_Pointer(P);
   end;

end RDF.Auxiliary.Convert_Void;
