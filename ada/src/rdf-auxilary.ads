with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
private with Ada.Strings.Unbounded;

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

   -- TODO: Use Containers.Indefinite_Holders instead?
   type String_Or_Null is private;

   function Null_Value return String_Or_Null;

   function From_String (Str: String) return String_Or_Null;

   function To_String (Value: String_Or_Null) return String
      with Pre => not Is_Null(Value);

   function Is_Null (Value: String_Or_Null) return Boolean;

   -- Allocates a new string (if not null)
   function New_String (Value: String_Or_Null) return chars_ptr;

   -- Writes null-terminated string into Data and return a pointer to it,
--     function To_C (Value: String_Or_Null; Data: aliased out char_array) return chars_ptr;

private

   -- Not sure that this is the most efficient implemenation
   type String_Or_Null (Has_Value: Boolean := False) is
      record
         case Has_Value is
            when False => null;
            when True  => Value: Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

end RDF.Auxilary;
