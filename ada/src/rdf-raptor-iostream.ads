with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxilary.Simple_Limited_Handled_Record;
with RDF.Raptor.World;

package RDF.Raptor.IOStream is

   -- It is impossible to make this as a wrapper around Ada.Streams.Root_Stream_Type,
   -- because Root_Stream_Type does not provide End_Of_File function.

   -- TODO: However, we can wrap this in Ada.Streams.Root_Stream_Type

   type Stream_Type_Without_Finalize is new RDF.Auxilary.Simple_Limited_Handled_Record.Base_Object with null record;

   subtype Handle_Type is RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type;

   -- TODO: stopped at raptor_new_iostream_from_sink ()

   type Stream_Type is new Stream_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle(Object: Stream_Type; Handle: Handle_Type);

   -- essentially abstract
   type User_Defined_Stream_Type is new Stream_Type with private;

   --overriding function Default_Handle(Object: User_Defined_Stream_Type) return Handle_Type;
   not overriding function Open (World: RDF.Raptor.World.World_Type) return User_Defined_Stream_Type;

   -- We can do initizization and finalization on Ada level.
   -- No need to provide such callbacks to the underlying C library

   --overriding procedure Initialize(Object: in out User_Defined_Stream_Type);

   --overriding procedure Finalize(Object: in out User_Defined_Stream_Type);

   -- TODO: We can implement this with Write_Bytes procedure
   not overriding procedure Do_Write_Byte (Stream: User_Defined_Stream_Type; Byte: char);

   not overriding procedure Do_Write_Bytes (Stream: User_Defined_Stream_Type; Data: chars_ptr; Size, Count: size_t);

   not overriding procedure Do_Write_End (Stream: User_Defined_Stream_Type);

   not overriding function Do_Read_Bytes (Stream: User_Defined_Stream_Type; Data: chars_ptr; Size, Count: size_t) return size_t;

   not overriding function Do_Read_Eof (Stream: User_Defined_Stream_Type) return Boolean;

private

   type User_Defined_Stream_Type is new Stream_Type with null record;

end RDF.Raptor.IOStream;
