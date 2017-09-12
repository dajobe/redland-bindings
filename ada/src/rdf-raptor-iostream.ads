with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Strings.Unbounded;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Auxiliary;
with RDF.Raptor.World;
limited with RDF.Raptor.URI;
limited with RDF.Raptor.Term;
limited with RDF.Raptor.Statement;

package RDF.Raptor.IOStream is

   -- It is impossible to make this as a wrapper around Ada.Streams.Root_Stream_Type,
   -- because Root_Stream_Type does not provide End_Of_File function.

   -- However, we can wrap this in Ada.Streams.Root_Stream_Type
   -- But this probably makes no sense for a user of our code.

   IOStream_Exception: exception;

   package Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype Handle_Type is Handled_Record.Access_Type;

   type Base_Stream_Type is new Handled_Record.Base_Object with null record;

   procedure Hexadecimal_Write (Value: Natural; Width: Natural; Stream: Base_Stream_Type);

   function Read_Bytes (Ptr: chars_ptr; size, nmemb: size_t; Stream: Base_Stream_Type) return size_t;

   function Read_Eof (Stream: Base_Stream_Type) return Boolean;

   function Tell (Stream: Base_Stream_Type) return unsigned_long;

   procedure Write (Value: String; Stream: Base_Stream_Type);

   procedure Decimal_Write (Value: int; Stream: Base_Stream_Type);

   procedure Write (Value: char; Stream: Base_Stream_Type);

   function Write_Bytes (Ptr: chars_ptr; size, nmemb: size_t; Stream: Base_Stream_Type) return int;

   procedure Write_End (Stream: Base_Stream_Type);

   procedure Bnodeid_Ntriples_Write (bnode: String; Stream: Base_Stream_Type);

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

   procedure Escaped_Write (Value: String; Delim: Character; Flags: Escaped_Write_Bitflags.Bitflags; Stream: Base_Stream_Type);

   procedure URI_Escaped_Write (URI, Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class;
                                Flags: Escaped_Write_Bitflags.Bitflags;
                                Stream: Base_Stream_Type);

   procedure Term_Escaped_Write (Term: RDF.Raptor.Term.Term_Type_Without_Finalize'Class; Flags: Escaped_Write_Bitflags.Bitflags; Stream: Base_Stream_Type);

   procedure Ntriples_Write (Value: String; Delim: Character; Stream: Base_Stream_Type);

   type Python_Write_Mode is (NTriples, Turtle, Turtle_Long_String, JSON)
      with Convention => C;
   for Python_Write_Mode use (NTriples=>0, Turtle=>1, Turtle_Long_String=>2, JSON=>3);

   procedure String_Python_Write (Value: String; Delim: Character; Mode: Python_Write_Mode; Stream: Base_Stream_Type);

   type Stream_Type_Without_Finalize is new Base_Stream_Type with null record;

   function From_Sink (World: RDF.Raptor.World.World_Type_Without_Finalize'Class) return Stream_Type_Without_Finalize;

   function From_Filename (World: RDF.Raptor.World.World_Type_Without_Finalize'Class; Filename: String) return Stream_Type_Without_Finalize;

   function From_File_Handle (World: RDF.Raptor.World.World_Type_Without_Finalize'Class; File: RDF.Auxiliary.C_File_Access)
                              return Stream_Type_Without_Finalize;

   -- See below type Stream_From_String instead
--     function From_String (World: RDF.Raptor.World.World_Type_Without_Finalize; Str: String)
--                           return Stream_Type_Without_Finalize;

   function To_Sink (World: RDF.Raptor.World.World_Type_Without_Finalize'Class) return Stream_Type_Without_Finalize;

   function To_Filename (World: RDF.Raptor.World.World_Type_Without_Finalize'Class; Filename: String) return Stream_Type_Without_Finalize;

   function To_File_Handle (World: RDF.Raptor.World.World_Type_Without_Finalize'Class; File: RDF.Auxiliary.C_File_Access)
                              return Stream_Type_Without_Finalize;

   -- See below type Stream_To_String instead
--     function To_String (World: RDF.Raptor.World.World_Type_Without_Finalize; Str: String)
--                         return Stream_Type_Without_Finalize;

   type Stream_Type is new Stream_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle(Object: Stream_Type; Handle: Handle_Type);

   -- essentially abstract
   type User_Defined_Stream_Type is new Base_Stream_Type with private;

   --overriding function Default_Handle(Object: User_Defined_Stream_Type) return Handle_Type;
   not overriding function Open (World: RDF.Raptor.World.World_Type_Without_Finalize'Class) return User_Defined_Stream_Type;

   -- We can do initizization and finalization on Ada level.
   -- No need to provide such callbacks to the underlying C library

   --overriding procedure Initialize(Object: in out User_Defined_Stream_Type);

   --overriding procedure Finalize(Object: in out User_Defined_Stream_Type);

   not overriding procedure Do_Write_Byte (Stream: in out User_Defined_Stream_Type; Byte: char);

   not overriding function Do_Write_Bytes (Stream: in out User_Defined_Stream_Type; Data: chars_ptr; Size, Count: size_t) return int;

   not overriding procedure Do_Write_End (Stream: in out User_Defined_Stream_Type);

   not overriding function Do_Read_Bytes (Stream: in out User_Defined_Stream_Type; Data: chars_ptr; Size, Count: size_t) return size_t;

   not overriding function Do_Read_Eof (Stream: in out User_Defined_Stream_Type) return Boolean;

   type Stream_From_String(<>) is new Base_Stream_Type with private;

   -- Hack to prevent compilation error:
   overriding function From_Handle(Handle: Handle_Type) return Stream_From_String;
   overriding function From_Non_Null_Handle(Handle: Handle_Type) return Stream_From_String;

   not overriding function Open_From_String (World: RDF.Raptor.World.World_Type_Without_Finalize'Class; Value: String) return Stream_From_String;

   -- I decided to implement it in Ada instead of using corresponding C functions
   type Stream_To_String is new Base_Stream_Type with private;

   function Open (World: RDF.Raptor.World.World_Type_Without_Finalize'Class) return Stream_To_String;

   not overriding function Value (Stream: Stream_To_String) return String;

private

   type Stream_From_String(Length: size_t) is new Base_Stream_Type with
      record
         Str: char_array(1..Length);
      end record;

   type User_Defined_Stream_Type is new Base_Stream_Type with null record;

   type Stream_To_String is new User_Defined_Stream_Type with
      record
         Str: Ada.Strings.Unbounded.Unbounded_String;
      end record;

   -- Hack to prevent compilation error:
   overriding function From_Handle(Handle: Handle_Type) return Stream_To_String;
   overriding function From_Non_Null_Handle(Handle: Handle_Type) return Stream_To_String;

   overriding function Do_Write_Bytes (Stream: in out Stream_To_String; Data: chars_ptr; Size, Count: size_t) return int;

end RDF.Raptor.IOStream;
