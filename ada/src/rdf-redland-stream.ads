with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.IOStream; use RDF.Raptor.IOStream;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.Node; use RDF.Redland.Node;
with RDF.Redland.Statement; use RDF.Redland.Statement;
with RDF.Redland.Node_Iterator; use RDF.Redland.Node_Iterator;

package RDF.Redland.Stream is

   package Stream_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Stream_Type_Without_Finalize is new Stream_Handled_Record.Base_Object with null record;

   subtype Stream_Handle is Stream_Handled_Record.Access_Type;

   not overriding function Is_End (Stream: Stream_Type_Without_Finalize) return Boolean;

   -- FIXME: What to do with the return value?
   -- TODO: Ada iterator
   not overriding procedure Next(Stream: Stream_Type_Without_Finalize);

   -- Should we instead return a copy of the statement?
   not overriding function Get_Object (Stream: Stream_Type_Without_Finalize)
                                       return Statement_Type_Without_Finalize;

   -- Should we instead return a copy of the node?
   not overriding function Get_Context (Stream: Stream_Type_Without_Finalize)
                                        return Node_Type_Without_Finalize;

   -- librdf_stream_add_map() not implemented

   not overriding procedure Write (Stream: Stream_Type_Without_Finalize; Raptor_Stream: Base_IOStream_Type'Class);

   package Finalizer is new With_Finalization(Stream_Type_Without_Finalize);

   type Stream_Type is new Finalizer.Derived with null record;

   overriding procedure Finalize_Handle (Object: Stream_Type; Handle: Stream_Handle);

   function Empty_Stream (World: Redland_World_Type_Without_Finalize'Class) return Stream_Type;

   -- FIXME: Check _Without_Finalize for Iterator
   not overriding function From_Node_Iterator (Iterator: Node_Iterator_Type;
                                               Statement: Statement_Type_Without_Finalize'Class;
                                               Field: Statement_Part_Flags)
                                               return Stream_Type;

end RDF.Redland.Stream;
