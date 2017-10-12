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

   type Iterator_Type is new Iterator_Type_Without_Finalize with null record;

   -- Stopped at librdf_iterator_get_object()

   overriding procedure Finalize_Handle (Object: Iterator_Type; Handle: Iterator_Handle);

end RDF.Redland.Iterator;
