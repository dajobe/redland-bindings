with Interfaces.C.Strings;

package RDF.Auxilary is

   -- Internal
   type Dummy_Record is null record;

   -- Internal
   type Dummy_Record_Access is access Dummy_Record;

   -- Unused. Remove?
   generic
      C_Ptr: Interfaces.C.Strings.chars_ptr;
   function C_Ptr_To_String return String;

end RDF.Auxilary;
