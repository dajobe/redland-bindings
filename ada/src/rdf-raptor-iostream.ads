with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Strings.Unbounded;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Auxiliary;
with RDF.Raptor.World; use RDF.Raptor.World;
limited with RDF.Raptor.URI;
limited with RDF.Raptor.Term;

package RDF.Raptor.IOStream is

   -- FIXME: Override Initialize_Handle to do nothing?

   -- It is impossible to make this as a wrapper around Ada.Streams.Root_IOStream_Type,
   -- because Root_IOStream_Type does not provide End_Of_File function.

   -- However, we can wrap this in Ada.Streams.Root_IOStream_Type
   -- But this probably makes no sense for a user of our code.

   IOStream_Exception: exception;

   package IOStream_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype IOStream_Handle is IOStream_Handled_Record.Access_Type;

   type IOStream_Type_Without_Finalize is new IOStream_Handled_Record.Base_Object with null record;

   overriding procedure Finalize_Handle (Object: IOStream_Type_Without_Finalize; Handle: IOStream_Handle);

   procedure Hexadecimal_Write (Value: Natural; Width: Natural; Stream: IOStream_Type_Without_Finalize);

   function Read_Bytes (Ptr: chars_ptr; size, nmemb: size_t; Stream: IOStream_Type_Without_Finalize) return size_t;

   function Read_Eof (Stream: IOStream_Type_Without_Finalize) return Boolean;

   function Tell (Stream: IOStream_Type_Without_Finalize) return unsigned_long;

   procedure Write (Value: String; Stream: IOStream_Type_Without_Finalize);

   procedure Decimal_Write (Value: int; Stream: IOStream_Type_Without_Finalize);

   procedure Write (Value: char; Stream: IOStream_Type_Without_Finalize);

   function Write_Bytes (Ptr: chars_ptr; size, nmemb: size_t; Stream: IOStream_Type_Without_Finalize) return int;

   procedure Write_End (Stream: IOStream_Type_Without_Finalize);

   procedure Bnodeid_Ntriples_Write (bnode: String; Stream: IOStream_Type_Without_Finalize);

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

   procedure Escaped_Write (Value: String; Delim: Character;
                            Flags: Escaped_Write_Bitflags.Bitflags;
                            Stream: IOStream_Type_Without_Finalize);

   procedure URI_Escaped_Write (URI, Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class;
                                Flags: Escaped_Write_Bitflags.Bitflags;
                                Stream: IOStream_Type_Without_Finalize);

   procedure Term_Escaped_Write (Term: RDF.Raptor.Term.Term_Type_Without_Finalize'Class;
                                 Flags: Escaped_Write_Bitflags.Bitflags;
                                 Stream: IOStream_Type_Without_Finalize);

   procedure Ntriples_Write (Value: String; Delim: Character; Stream: IOStream_Type_Without_Finalize);

   type Python_Write_Mode is (NTriples, Turtle, Turtle_Long_String, JSON)
     with Convention => C;
   for Python_Write_Mode use (NTriples=>0, Turtle=>1, Turtle_Long_String=>2, JSON=>3);

   procedure String_Python_Write (Value: String;
                                  Delim: Character;
                                  Mode: Python_Write_Mode;
                                  Stream: IOStream_Type_Without_Finalize);

   package Handlers is new IOStream_Handled_Record.Common_Handlers(IOStream_Type_Without_Finalize);

   type IOStream_Type is new Handlers.Base_With_Finalization with null record;

   type IOStream_Type_User is new Handlers.User_Type with null record;

   function From_Sink (World: Raptor_World_Type_Without_Finalize'Class) return IOStream_Type;

   function From_Filename (World: Raptor_World_Type_Without_Finalize'Class; Filename: String)
                           return IOStream_Type;

   function From_File_Handle (World: Raptor_World_Type_Without_Finalize'Class;
                              File: RDF.Auxiliary.C_File_Access)
                              return IOStream_Type;

   -- See below type Stream_From_String instead
   --     function From_String (World: Raptor_World_Type_Without_Finalize; Str: String)
   --                           return IOStream_Type_Without_Finalize;

   function To_Sink (World: Raptor_World_Type_Without_Finalize'Class) return IOStream_Type;

   function To_Filename (World: Raptor_World_Type_Without_Finalize'Class; Filename: String)
                         return IOStream_Type;

   function To_File_Handle (World: Raptor_World_Type_Without_Finalize'Class; File: RDF.Auxiliary.C_File_Access)
                            return IOStream_Type;

   -- See below type Stream_To_String instead
   --     function To_String (World: Raptor_World_Type_Without_Finalize; Str: String)
   --                         return IOStream_Type_Without_Finalize;

   type Handled_IOStream_Type_User is new IOStream_Type_User with null record;

   --overriding function Default_Handle(Object: IOStream_Type_User) return Handle_Type;
   not overriding function Open (World: Raptor_World_Type_Without_Finalize'Class)
                                 return Handled_IOStream_Type_User;

   -- We can do initizization and finalization on Ada level.
   -- No need to provide such callbacks to the underlying C library

   --overriding procedure Initialize(Object: in out IOStream_Type_User);

   --overriding procedure Finalize(Object: in out IOStream_Type_User);

   not overriding procedure Do_Write_Byte (Stream: in out Handled_IOStream_Type_User; Byte: char);

   not overriding function Do_Write_Bytes (Stream: in out Handled_IOStream_Type_User;
                                           Data: chars_ptr;
                                           Size, Count: size_t)
                                           return int;

   not overriding procedure Do_Write_End (Stream: in out Handled_IOStream_Type_User);

   not overriding function Do_Read_Bytes (Stream: in out Handled_IOStream_Type_User;
                                          Data: chars_ptr;
                                          Size, Count: size_t) return size_t;

   not overriding function Do_Read_Eof (Stream: in out Handled_IOStream_Type_User) return Boolean;

   type Stream_From_String(<>) is new IOStream_Type_User with private;

   -- Hack to prevent compilation error:
   overriding function From_Handle(Handle: IOStream_Handle) return Stream_From_String;
   overriding function From_Non_Null_Handle(Handle: IOStream_Handle) return Stream_From_String;

   not overriding function Open_From_String (World: Raptor_World_Type_Without_Finalize'Class; Value: String)
                                             return Stream_From_String;

   -- I decided to implement it in Ada instead of using corresponding C functions
   type Stream_To_String is new IOStream_Type_User with private;

   function Open (World: Raptor_World_Type_Without_Finalize'Class) return Stream_To_String;

   not overriding function Value (Stream: Stream_To_String) return String;

private

   type Stream_From_String(Length: size_t) is new IOStream_Type_User with
      record
         Str: char_array(1..Length);
      end record;

   type Stream_To_String is new Handled_IOStream_Type_User with
      record
         Str: Ada.Strings.Unbounded.Unbounded_String;
      end record;

   -- Hack to prevent compilation error:
   overriding function From_Handle(Handle: IOStream_Handle) return Stream_To_String;
   overriding function From_Non_Null_Handle(Handle: IOStream_Handle) return Stream_To_String;

   overriding function Do_Write_Bytes (Stream: in out Stream_To_String; Data: chars_ptr; Size, Count: size_t)
                                       return int;

end RDF.Raptor.IOStream;
