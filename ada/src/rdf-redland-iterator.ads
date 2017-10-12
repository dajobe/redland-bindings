with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.Limited_Handled_Record;

package RDF.Redland.Iterator is

   -- TODO: Necessarily test that it works as expected

   package Iterator_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Iterator_Type_Without_Finalize is new Iterator_Handled_Record.Base_Object with null record;

   subtype Iterator_Handle is Iterator_Handled_Record.Access_Type;

   -- TODO: librdf_iterator_map_handler, librdf_iterator_map_free_context_handler,
   -- librdf_new_iterator() are unimplemented

   not overriding function Is_End (Iterator: Iterator_Type_Without_Finalize) return Boolean;

   not overriding procedure Next (Iterator: in out Iterator_Type_Without_Finalize);

   -- Internal functions
   not overriding function Get_Object_Internal  (Iterator: Iterator_Type_Without_Finalize) return chars_ptr;
   not overriding function Get_Context_Internal (Iterator: Iterator_Type_Without_Finalize) return chars_ptr;
   not overriding function Get_Key_Internal     (Iterator: Iterator_Type_Without_Finalize) return chars_ptr;
   not overriding function Get_Value_Internal   (Iterator: Iterator_Type_Without_Finalize) return chars_ptr;

   -- librdf_iterator_add_map() not implemented

   -- Stopped at librdf_new_empty_iterator()

   type Iterator_Type is new Iterator_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Object: Iterator_Type; Handle: Iterator_Handle);

   not overriding function Empty_Iterator return Iterator_Type;

end RDF.Redland.Iterator;
