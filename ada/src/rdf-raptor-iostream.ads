-- TODO

with RDF.Auxilary.Simple_Handled_Record;

package RDF.Raptor.IOStream is

   -- It is impossible to make this as a wrapper around Ada.Streams.Root_Stream_Type,
   -- because Root_Stream_Type does not provide End_Of_File function.

   -- TODO: However, we can wrap this in Ada.Streams.Root_Stream_Type

   type Stream_Type is new RDF.Auxilary.Simple_Handled_Record.Base_Object with null record;

   subtype Handle_Type is RDF.Auxilary.Simple_Handled_Record.Access_Type;

end RDF.Raptor.IOStream;
