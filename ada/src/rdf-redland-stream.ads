with Ada.Containers;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.IOStream; use RDF.Raptor.IOStream;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.Node; use RDF.Redland.Node;
with RDF.Redland.Statement; use RDF.Redland.Statement;
with RDF.Redland.Node_Iterator; use RDF.Redland.Node_Iterator;
with Ada.Iterator_Interfaces;

package RDF.Redland.Stream is

   package Stream_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Stream_Type_Without_Finalize is new Stream_Handled_Record.Base_Object with null record;

   subtype Stream_Handle is Stream_Handled_Record.Access_Type;

   overriding procedure Finalize_Handle (Object: Stream_Type_Without_Finalize; Handle: Stream_Handle);

   not overriding function Is_End (Stream: Stream_Type_Without_Finalize) return Boolean;

   not overriding procedure Next(Stream: Stream_Type_Without_Finalize);

   -- Should we instead return a copy of the statement?
   not overriding function Get_Object (Stream: Stream_Type_Without_Finalize)
                                       return Statement_Type_Without_Finalize;

   -- Should we instead return a copy of the node?
   not overriding function Get_Context (Stream: Stream_Type_Without_Finalize)
                                        return Node_Type_Without_Finalize;

   -- librdf_stream_add_map() not implemented

   not overriding procedure Write (Stream: Stream_Type_Without_Finalize; Raptor_Stream: IOStream_Type_Without_Finalize'Class);

   type Statement_Array is array (Ada.Containers.Count_Type range <>) of Statement_Type;

   -- Note that after this operation Stream is not usable
   not overriding function To_Array (Stream: in out Stream_Type_Without_Finalize)
                                     return Statement_Array;

   package Handlers is new Stream_Handled_Record.Common_Handlers(Stream_Type_Without_Finalize);

   type Stream_Type is new Handlers.Base_With_Finalization with null record;

   type Stream_Type_User is new Handlers.User_Type with null record;

   function Empty_Stream (World: Redland_World_Type_Without_Finalize'Class) return Stream_Type;

   not overriding function From_Node_Iterator (Iterator: Node_Iterator_Type_Without_Finalize'Class;
                                               Statement: Statement_Type_Without_Finalize'Class;
                                               Field: Statement_Part_Flags)
                                               return Stream_Type;

   type Cursor is private;

   not overriding function Has_Element (Position: Cursor) return Boolean;

   package Base_Iterators is new Ada.Iterator_Interfaces(Cursor, Has_Element);

   type Stream_Iterator is new Base_Iterators.Forward_Iterator with private;

   not overriding function Create_Stream_Iterator (Stream: in out Stream_Type_Without_Finalize'Class)
                                                   return Stream_Iterator;

   overriding function First (Object: Stream_Iterator) return Cursor;

   overriding function Next (Object: Stream_Iterator; Position: Cursor) return Cursor;

private

   type Cursor is access constant Stream_Type_Without_Finalize'Class;

   type Stream_Iterator is new Base_Iterators.Forward_Iterator with
      record
         Ref: Cursor;
      end record;

end RDF.Redland.Stream;
