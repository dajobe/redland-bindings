with Interfaces.C; use Interfaces.C;
with RDF.Redland.Iterator; use RDF.Redland.Iterator;

package body RDF.Redland.Stream is

   function librdf_new_stream_from_node_iterator (Iterator: Iterator_Handle;
                                                  Statement: Statement_Handle;
                                                  Field: Statement_Part_Flags)
                                                  return Stream_Handle
     with Import, Convention=>C;

   function From_Node_Iterator (Iterator: Node_Iterator_Type;
                                Statement: Statement_Type_Without_Finalize'Class;
                                Field: Statement_Part_Flags)
                                return Stream_Type is
      Handle: constant Stream_Handle :=
        librdf_new_stream_from_node_iterator(Get_Handle(Iterator), Get_Handle(Statement), Field);
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_new_empty_stream (World: Redland_World_Handle) return Stream_Handle
     with Import, Convention=>C;

   function Empty_Stream (World: Redland_World_Type_Without_Finalize'Class) return Stream_Type is
   begin
      return From_Non_Null_Handle(librdf_new_empty_stream(Get_Handle(World)));
   end;

   procedure librdf_free_stream (Stream: Stream_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Stream_Type; Handle: Stream_Handle) is
   begin
      librdf_free_stream(Handle);
   end;

   function librdf_stream_end (Stream: Stream_Handle) return int
     with Import, Convention=>C;

   function Is_End (Stream: Stream_Type_Without_Finalize) return Boolean is
   begin
      return librdf_stream_end(Get_Handle(Stream)) /= 0;
   end;

   function librdf_stream_next (Stream: Stream_Handle) return int
     with Import, Convention=>C;

   procedure Next(Stream: Stream_Type_Without_Finalize) is
      Result: constant int := librdf_stream_next(Get_Handle(Stream));
      pragma Unreferenced(Result);
   begin
      null;
   end;

   function librdf_stream_get_object (Stream: Stream_Handle) return Statement_Handle
     with Import, Convention=>C;

   function Get_Object (Stream: Stream_Type_Without_Finalize) return Statement_Type_Without_Finalize is
   begin
      return From_Handle(librdf_stream_get_object(Get_Handle(Stream)));
   end;

   function librdf_stream_get_context2 (Stream: Stream_Handle) return Node_Handle
     with Import, Convention=>C;

   function Get_Context (Stream: Stream_Type_Without_Finalize) return Node_Type_Without_Finalize is
   begin
      return From_Handle(librdf_stream_get_context2(Get_Handle(Stream)));
   end;

   function librdf_stream_write (Stream: Stream_Handle; Raptor_Stream: IOStream_Handle) return int
     with Import, Convention=>C;

   procedure Write (Stream: Stream_Type_Without_Finalize; Raptor_Stream: Base_IOStream_Type'Class) is
   begin
      if librdf_stream_write(Get_Handle(Stream), Get_Handle(Raptor_Stream)) /= 0 then
         raise RDF.Raptor.IOStream.IOStream_Exception;
      end if;
   end;

end RDF.Redland.Stream;
