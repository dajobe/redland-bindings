with Ada.Iterator_Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;

package RDF.Raptor.Syntaxes is

   type Syntax_Bitflags is (Need_Base_URI)
      with Convention => C;
   for Syntax_Bitflags use (Need_Base_URI => 1);

   type Mime_Type_Q is private;

   -- Internally represented as an access to a C record. (It is used for Ada.Unchecked_Conversion in some other packages.)
   type Syntax_Description_Type is private;

   type Q_Type is range 0..10;

   function Get_MIME_Type (Object: Mime_Type_Q) return String;

   function Get_Q (Object: Mime_Type_Q) return Q_Type;

   function Get_Name (Object: Syntax_Description_Type; Index: Natural) return String
      with Pre => Index < Get_Names_Count(Object);

   function Get_Names_Count (Object: Syntax_Description_Type) return Natural;

   function Get_Label (Object: Syntax_Description_Type) return String;

   function Get_MIME_Type (Object: Syntax_Description_Type; Index: Natural) return Mime_Type_Q
      with Pre => Index < Get_MIME_Types_Count(Object);

   function Get_MIME_Types_Count (Object: Syntax_Description_Type) return Natural;

   function Get_URI (Object: Syntax_Description_Type; Index: Natural) return URI_String
      with Pre => Index < Get_URIs_Count(Object);

   function Get_URIs_Count (Object: Syntax_Description_Type) return Natural;

   function Get_Flags (Object: Syntax_Description_Type) return Syntax_Bitflags;

   -- raptor_syntax_description_validate() deliberately not implemented

   type Parser_Description_Cursor     is private;
   type Serializer_Description_Cursor is private;

   function Get_Position (Cursor: Parser_Description_Cursor    ) return Natural;
   function Get_Position (Cursor: Serializer_Description_Cursor) return Natural;

   function Get_Description (Cursor: Parser_Description_Cursor    ) return Syntax_Description_Type;
   function Get_Description (Cursor: Serializer_Description_Cursor) return Syntax_Description_Type;

   function Has_Element (Position: Parser_Description_Cursor    ) return Boolean;
   function Has_Element (Position: Serializer_Description_Cursor) return Boolean;

   package Parser_Description_Iterators     is new Ada.Iterator_Interfaces(Parser_Description_Cursor    , Has_Element);
   package Serializer_Description_Iterators is new Ada.Iterator_Interfaces(Serializer_Description_Cursor, Has_Element);

   type Parser_Description_Iterator     is new Parser_Description_Iterators    .Forward_Iterator with private;
   type Serializer_Description_Iterator is new Serializer_Description_Iterators.Forward_Iterator with private;

   overriding function First (Object: Parser_Description_Iterator) return Parser_Description_Cursor;
   overriding function Next (Object: Parser_Description_Iterator; Position: Parser_Description_Cursor) return Parser_Description_Cursor;

   overriding function First (Object: Serializer_Description_Iterator) return Serializer_Description_Cursor;
   overriding function Next (Object: Serializer_Description_Iterator; Position: Serializer_Description_Cursor) return Serializer_Description_Cursor;

   not overriding function Create_Parser_Descriptions_Iterator     (World: Raptor_World_Type_Without_Finalize'Class) return Parser_Description_Iterator;
   not overriding function Create_Serializer_Descriptions_Iterator (World: Raptor_World_Type_Without_Finalize'Class) return Serializer_Description_Iterator;

   function Is_Parser_Name (World: Raptor_World_Type_Without_Finalize'Class; Name: String) return Boolean;

   function Guess_Parser_Name (World: Raptor_World_Type_Without_Finalize'Class; URI: URI_Type; MIME_Type: String; Buffer: String; Identifier: String)
                               return String;

   function Is_Serializer_Name (World: Raptor_World_Type_Without_Finalize'Class; Name: String) return Boolean;

private

   type Mime_Type_Q is
      record
         Mime_Type: chars_ptr;
         Mime_Type_Len: size_t;
         Q: unsigned_char;
      end record
      with Convention => C;

   type Syntax_Description_Record is limited
      record
         Names: access chars_ptr;
         Names_Count: unsigned;
         Label: chars_ptr;
         Mime_Types: access Mime_Type_Q;
         Mime_Types_Count: unsigned;
         URI_Strings: access chars_ptr;
         URI_Strings_Count: unsigned;
         Flags: Syntax_Bitflags;
      end record
     with Convention => C;

   type Syntax_Description_Type is access constant Syntax_Description_Record
     with Convention => C;

   type Parser_Description_Cursor is
      record
         World: Raptor_World_Handle;
         Position: Natural;
      end record;

   type Serializer_Description_Cursor is
      record
         World: Raptor_World_Handle;
         Position: Natural;
      end record;

   type Parser_Description_Iterator is new Parser_Description_Iterators.Forward_Iterator with
      record
         World: Raptor_World_Handle;
      end record;

   type Serializer_Description_Iterator is new Serializer_Description_Iterators.Forward_Iterator with
      record
         World: Raptor_World_Handle;
      end record;

   type Parser_Descriptions_List is tagged
      record
         World: Raptor_World_Handle;
      end record;

   type Serializer_Descriptions_List is tagged
      record
         World: Raptor_World_Handle;
      end record;

end RDF.Raptor.Syntaxes;
