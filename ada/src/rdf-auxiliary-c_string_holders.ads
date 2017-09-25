with Ada.Containers.Indefinite_Holders;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package RDF.Auxiliary.C_String_Holders is

   package Char_Array_Holders is new Ada.Containers.Indefinite_Holders(char_array);

   type C_String_Holder is new Char_Array_Holders.Holder with null record;

   function Length (Object: C_String_Holder) return size_t;

   function New_String (Value: String_Holders.Holder) return chars_ptr;

   function New_Holder (Ptr: chars_ptr) return String_Holders.Holder;

   function To_C_String_Holder (Item: String_Holders.Holder) return C_String_Holder;

   function C_String (Object: C_String_Holder) return chars_ptr;

end RDF.Auxiliary.C_String_Holders;
