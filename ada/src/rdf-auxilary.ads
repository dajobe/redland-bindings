with Interfaces.C.Strings;

package RDF.Auxilary is

   -- Internal
   type Dummy_Record is null record
      with Convention=>C;

   type Comparison_Result is range -1..1;

   -- TODO: Unused. Remove?
   generic
      C_Ptr: Interfaces.C.Strings.chars_ptr;
   function C_Ptr_To_String return String;

end RDF.Auxilary;
