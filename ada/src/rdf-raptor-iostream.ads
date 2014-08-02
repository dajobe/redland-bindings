with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxilary.Simple_Limited_Handled_Record;
with RDF.Raptor.World;

package RDF.Raptor.IOStream is

   -- It is impossible to make this as a wrapper around Ada.Streams.Root_Stream_Type,
   -- because Root_Stream_Type does not provide End_Of_File function.

   -- TODO: However, we can wrap this in Ada.Streams.Root_Stream_Type

   IOStream_Exception: exception;

   type Stream_Type_Without_Finalize is new RDF.Auxilary.Simple_Limited_Handled_Record.Base_Object with null record;

   subtype Handle_Type is RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type;

   function From_Sink (World: RDF.Raptor.World.World_Type_Without_Finalize) return Stream_Type_Without_Finalize;

   function From_Filename (World: RDF.Raptor.World.World_Type_Without_Finalize; Filename: String) return Stream_Type_Without_Finalize;

   function From_File_Handle (World: RDF.Raptor.World.World_Type_Without_Finalize; File: RDF.Auxilary.C_File_Access)
                              return Stream_Type_Without_Finalize;

   function From_String (World: RDF.Raptor.World.World_Type_Without_Finalize; Str: String)
                         return Stream_Type_Without_Finalize;

   function To_Sink (World: RDF.Raptor.World.World_Type_Without_Finalize) return Stream_Type_Without_Finalize;

   function To_Filename (World: RDF.Raptor.World.World_Type_Without_Finalize; Filename: String) return Stream_Type_Without_Finalize;

   function To_File_Handle (World: RDF.Raptor.World.World_Type_Without_Finalize; File: RDF.Auxilary.C_File_Access)
                              return Stream_Type_Without_Finalize;

   function To_String (World: RDF.Raptor.World.World_Type_Without_Finalize; Str: String)
                         return Stream_Type_Without_Finalize;

   procedure Hexadecimal_Write (Value: Natural; Width: Natural; Stream: Stream_Type_Without_Finalize);

   function Read_Bytes (Ptr: chars_ptr; size, nmemb: size_t; Stream: Stream_Type_Without_Finalize) return size_t;

   function Read_Eof (Stream: Stream_Type_Without_Finalize) return Boolean;

   function Tell (Stream: Stream_Type_Without_Finalize) return unsigned_long;

   procedure Write (Value: String; Stream: Stream_Type_Without_Finalize);

   procedure Decimal_Write (Value: int; Stream: Stream_Type_Without_Finalize);

   procedure Write (Value: char; Stream: Stream_Type_Without_Finalize);

   procedure Write_Bytes (Ptr: chars_ptr; size, nmemb: size_t; Stream: Stream_Type_Without_Finalize);

   procedure Write_End (Stream: Stream_Type_Without_Finalize);

   procedure Bnodeid_Ntriples_Write (bnode: String; Stream: Stream_Type_Without_Finalize);

   package Escaped_Write_Bitflags is

      type Bitflags is mod 256; -- the number may change in a future version

      BITFLAG_BS_ESCAPES_BF:      constant Bitflags := 1;
      BITFLAG_BS_ESCAPES_TNRU:    constant Bitflags := 2;
      BITFLAG_UTF8:               constant Bitflags := 4;
      BITFLAG_SPARQL_URI_ESCAPES: constant Bitflags := 8;

      -- N-Triples - favour writing \u, \U over UTF8
      NTRIPLES_LITERAL: constant Bitflags := BITFLAG_BS_ESCAPES_TNRU or BITFLAG_BS_ESCAPES_BF;
      NTRIPLES_URI    : constant Bitflags := BITFLAG_SPARQL_URI_ESCAPES;

      -- SPARQL literal: allows raw UTF8 for printable literals
      SPARQL_LITERAL: constant Bitflags := BITFLAG_UTF8;

      -- SPARQL long literal: no BS-escapes allowe
      SPARQL_LONG_LITERAL: constant Bitflags := BITFLAG_UTF8;

      -- SPARQL uri: have to escape certain characters
      SPARQL_URI: constant Bitflags := BITFLAG_UTF8 or BITFLAG_SPARQL_URI_ESCAPES;

      -- Turtle (2013) escapes are like SPARQL
      TURTLE_URI:          constant Bitflags := SPARQL_URI;
      TURTLE_LITERAL:      constant Bitflags := SPARQL_LITERAL;
      TURTLE_LONG_LITERAL: constant Bitflags := SPARQL_LONG_LITERAL;

      --- JSON literals: \b \f \t \r \n and \u \U
      JSON_LITERAL: constant Bitflags := BITFLAG_BS_ESCAPES_TNRU or BITFLAG_BS_ESCAPES_BF;

   end Escaped_Write_Bitflags;

   procedure Escaped_Write (Value: String; Delim: Character; Flags: Escaped_Write_Bitflags.Bitflags; Stream: Stream_Type_Without_Finalize);

   -- TODO:
   -- raptor_term_escaped_write(), raptor_uri_escaped_write()

   procedure Ntriples_Write (Value: String; Delim: Character; Stream: Stream_Type_Without_Finalize);

   type Python_Write_Mode is (NTriples, Turtle, Turtle_Long_String, JSON);
   for Python_Write_Mode use (NTriples=>0, Turtle=>1, Turtle_Long_String=>2, JSON=>3);
   for Python_Write_Mode'Size use int'Size; -- hack

   procedure String_Python_Write (Value: String; Delim: Character; Mode: Python_Write_Mode; Stream: Stream_Type_Without_Finalize);

   type Stream_Type is new Stream_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle(Object: Stream_Type; Handle: Handle_Type);

   -- essentially abstract
   type User_Defined_Stream_Type is new Stream_Type with private;

   --overriding function Default_Handle(Object: User_Defined_Stream_Type) return Handle_Type;
   not overriding function Open (World: RDF.Raptor.World.World_Type_Without_Finalize) return User_Defined_Stream_Type;

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
