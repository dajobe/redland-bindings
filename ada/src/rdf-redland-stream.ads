with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.Statement; use RDF.Redland.Statement;
with RDF.Redland.Node_Iterator; use RDF.Redland.Node_Iterator;

package RDF.Redland.Stream is

   package Stream_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Stream_Type_Without_Finalize is new Stream_Handled_Record.Base_Object with null record;

   subtype Stream_Handle is Stream_Handled_Record.Access_Type;

   -- TODO: Stopped at librdf_stream_end()

   type Stream_Type is new Stream_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Object: Stream_Type; Handle: Stream_Handle);

   function Empty_Stream (World: Redland_World_Type_Without_Finalize'Class) return Stream_Type;

   -- FIXME: Check _Without_Finalize for Iterator
   not overriding function From_Node_Iterator (Iterator: Node_Iterator_Type;
                                               Statement: Statement_Type_Without_Finalize'Class;
                                               Field: Statement_Part_Flags)
                                               return Stream_Type;

end RDF.Redland.Stream;
