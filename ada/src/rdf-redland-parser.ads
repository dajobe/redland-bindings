with Ada.Iterator_Interfaces;
with RDF.Auxiliary; use RDF.Auxiliary;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.IOStream; use RDF.Raptor.IOStream;
with RDF.Raptor.Syntaxes; use RDF.Raptor.Syntaxes;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.URI; use RDF.Redland.URI;
with RDF.Redland.Node; use RDF.Redland.Node;
with RDF.Redland.Stream; use RDF.Redland.Stream;
with RDF.Redland.Model; use RDF.Redland.Model;

package RDF.Redland.Parser is

   package Parser_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Parser_Type_Without_Finalize is new Parser_Handled_Record.Base_Object with null record;

   subtype Parser_Handle is Parser_Handled_Record.Access_Type;

   function Parser_Check_Name (World: Redland_World_Type_Without_Finalize'Class; Name: String)
                               return Boolean;

   -- Order of arguments not the same as in C
   function Parser_Guess_Name (World: Redland_World_Type_Without_Finalize'Class;
                               Mime_Type, Identifier: String;
                               Buffer: String_Holders.Holder := String_Holders.Empty_Holder)
                               return String;

   function Get_Parser_Description (World: Redland_World_Type_Without_Finalize'Class;
                                    Counter: Natural)
                                    return Raptor_Syntax_Description_Type;

   type Parser_Description_Cursor is private;

   function Get_Position (Cursor: Parser_Description_Cursor) return Natural;

   function Get_Description (Cursor: Parser_Description_Cursor) return Raptor_Syntax_Description_Type;

   function Has_Element (Position: Parser_Description_Cursor) return Boolean;

   package Parser_Description_Iterators is new Ada.Iterator_Interfaces(Parser_Description_Cursor, Has_Element);

   type Parser_Description_Iterator is new Parser_Description_Iterators.Forward_Iterator with private;

   overriding function First (Object: Parser_Description_Iterator) return Parser_Description_Cursor;
   overriding function Next (Object: Parser_Description_Iterator; Position: Parser_Description_Cursor)
                             return Parser_Description_Cursor;

   function Create_Parser_Descriptions_Iterator (World: Redland_World_Type_Without_Finalize'Class)
                                                 return Parser_Description_Iterator;

   not overriding function As_Stream (Parser: Parser_Type_Without_Finalize;
                                      URI: URI_Type_Without_Finalize'Class;
                                      Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                                      return Stream_Type;

   -- order of arguments differs of C function
   not overriding procedure Parse_Into_Model (Parser: Parser_Type_Without_Finalize;
                                              Model: in out Model_Type_Without_Finalize'Class;
                                              URI: URI_Type_Without_Finalize'Class;
                                              Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)));

   not overriding
   function Parse_File_Handle_As_Stream (Parser: Parser_Type_Without_Finalize;
                                         File: RDF.Auxiliary.C_File_Access;
                                         Close: Boolean;
                                         Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                                         return Stream_Type;

   -- order of arguments differs of C function
   not overriding
   procedure Parse_File_Handle_Into_Model (Parser: Parser_Type_Without_Finalize;
                                           File: RDF.Auxiliary.C_File_Access;
                                           Close: Boolean;
                                           Model: in out Model_Type_Without_Finalize'Class;
                                           Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)));

   not overriding
   function Parse_String_As_Stream (Parser: Parser_Type_Without_Finalize;
                                    Text: String;
                                    Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                                    return Stream_Type
     with Pre => (Text /= "");

   -- order of arguments differs of C function
   not overriding
   procedure Parse_String_Into_Model (Parser: Parser_Type_Without_Finalize;
                                      Model: Model_Type_Without_Finalize'Class;
                                      Text: String;
                                      Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)));

   not overriding
   function Parse_IOStream_As_Stream (Parser: Parser_Type_Without_Finalize;
                                      IOStream: Base_IOStream_Type'Class;
                                      Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                                      return Stream_Type;

   -- order of arguments differs of C function
   not overriding
   procedure Parse_IOStream_Into_Model (Parser: Parser_Type_Without_Finalize;
                                        Model: Model_Type_Without_Finalize'Class;
                                        IOStream: Base_IOStream_Type'Class;
                                        Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)));

   FEATURE_ERROR_COUNT  : URI_String := "http://feature.librdf.org/parser-error-count";
   FEATURE_WARNING_COUNT: URI_String := "http://feature.librdf.org/parser-warning-count";

   not overriding
   function Get_Feature (Parser: Parser_Type_Without_Finalize; Feature: URI_Type_Without_Finalize'Class)
                         return Node_Type;

   not overriding procedure Set_Feature (Parser: Parser_Type_Without_Finalize;
                                         Feature: URI_Type_Without_Finalize'Class;
                                         Value: Node_Type'Class);

   not overriding function Get_Accept_Header (Parser: Parser_Type_Without_Finalize) return String;

   -- librdf_parser_get_namespaces_seen_count(),
   -- librdf_parser_get_namespaces_seen_prefix(),
   -- librdf_parser_get_namespaces_seen_uri()
   -- not bound as internals

   -- TODO: Stopped at librdf_parser_get_uri_filter()

   type Parser_Type is new Parser_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Object: Parser_Type; Handle: Parser_Handle);

   not overriding function Create (World: Redland_World_Type_Without_Finalize'Class;
                                   Name, Mime_Type: String := "";
                                   Type_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                                   return Parser_Type;

private

   type Parser_Description_Cursor is
      record
         World: Redland_World_Handle;
         Position: Natural;
      end record;

   type Parser_Description_Iterator is new Parser_Description_Iterators.Forward_Iterator with
      record
         World: Redland_World_Handle;
      end record;

end RDF.Redland.Parser;
