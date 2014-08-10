with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
--  private with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Holders;

package RDF.Auxilary is

   -- Internal
   type Dummy_Record is null record
      with Convention=>C;

   type C_File_Record is null record
      with Convention=>C;

   -- Represents C FILE* record
   type C_File_Access is access C_File_Record
      with Convention=>C;

   type Comparison_Result is range -1..1;

   package String_Holders is new Ada.Containers.Indefinite_Holders(String);
--     subtype String_Or_Null is String_Holders.Holder;

   -- Allocates a new string (if not null)
   function New_String (Value: String_Holders.Holder) return chars_ptr;

   -- Writes null-terminated string into Data and return a pointer to it,
--     function To_C (Value: String_Or_Null; Data: aliased out char_array) return chars_ptr;

   type C_String_Result is private;

   function To_C_Object (Item: String_Holders.Holder) return C_String_Result;

--     function Get_C_String (Object: C_String_Result) return Chars_Ptr;

   function Length (Object: C_String_Result) return size_t;

private

   type My_Array is new char_array;

   package Char_Array_Holders is new Ada.Containers.Indefinite_Holders(char_array);

   type C_String_Result is new Char_Array_Holders.Holder with null record;

end RDF.Auxilary;
