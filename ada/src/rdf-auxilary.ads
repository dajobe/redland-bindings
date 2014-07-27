with Interfaces.C.Strings;

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

   -- TODO: Unused. Remove?
   generic
      C_Ptr: Interfaces.C.Strings.chars_ptr;
   function C_Ptr_To_String return String;

end RDF.Auxilary;
