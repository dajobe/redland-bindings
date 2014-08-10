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

   function Length (Object: String_Holders.Holder) return size_t;

private

   type My_Array is new char_array;

end RDF.Auxilary;
