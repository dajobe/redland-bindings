with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;

package RDF.Raptor.Syntaxes is

   type Syntax_Bitflags is (Need_Base_URI)
      with Convention => C;
   for Syntax_Bitflags use (Need_Base_URI => 1);

   type Mime_Type_Q is private;

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

   -- TODO: Subtype for URI strings
   function Get_URI (Object: Syntax_Description_Type; Index: Natural) return String
      with Pre => Index < Get_URIs_Count(Object);

   function Get_URIs_Count (Object: Syntax_Description_Type) return Natural;

   function Get_Flags (Object: Syntax_Description_Type) return Syntax_Bitflags;

   -- raptor_syntax_description_validate() deliberately not implemented

   -- TODO: raptor_world_get_parser_description () and raptor_world_get_serializer_description () - how to check for allowed counter range?

   function Is_Parser_Name (World: World_Type_Without_Finalize'Class; Name: String) return Boolean;

   function Guess_Parser_Name (World: World_Type_Without_Finalize'Class; URI: URI_Type; MIME_Type: String; Buffer: String; Identifier: String)
                               return String;

   function Is_Serializer_Name (World: World_Type_Without_Finalize'Class; Name: String) return Boolean;

private

   type Mime_Type_Q is
      record
         Mime_Type: chars_ptr;
         Mime_Type_Len: size_t;
         Q: unsigned_char;
      end record
      with Convention => C;

   type Syntax_Description_Type is
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

end RDF.Raptor.Syntaxes;
