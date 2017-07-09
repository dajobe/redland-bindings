with Interfaces.C; use Interfaces.C;
-- with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary; use RDF.Auxiliary;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Auxiliary.C_String_Holders; use RDF.Auxiliary.C_String_Holders;
with RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.IOStream;
with RDF.Raptor.Statement;
with RDF.Raptor.Namespaces;
with RDF.Raptor.Syntaxes;
with RDF.Raptor.Log;
with RDF.Raptor.WWW;
with RDF.Raptor.Options;

package RDF.Raptor.Parser is

   package Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype Handle_Type is Handled_Record.Access_Type;

   type Parser_Type_Without_Finalize is new Handled_Record.Base_Object with null record;

   type Graph_Mark_Flags is (Graph_Mark_Start, Graph_Mark_Declared);
   for Graph_Mark_Flags'Size use int'Size; -- hack
   for Graph_Mark_Flags use (Graph_Mark_Start=>1, Graph_Mark_Declared=>2);

   -- You can call this function or initialize only callbacks you need (below).
   not overriding procedure Initialize_All_Callbacks (Parser: Parser_Type_Without_Finalize);

   not overriding procedure Initialize_Graph_Mark_Handler (Object: Parser_Type_Without_Finalize);
   not overriding procedure Initialize_Statement_Handler  (Object: Parser_Type_Without_Finalize);
   not overriding procedure Initialize_Namespace_Handler  (Object: Parser_Type_Without_Finalize);

   not overriding procedure Graph_Mark_Handler (Object: Parser_Type_Without_Finalize;
                                                URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class;
                                                Flags: Graph_Mark_Flags) is null;

   not overriding procedure Statement_Handler (Object: Parser_Type_Without_Finalize;
                                               Statement: RDF.Raptor.Statement.Statement_Type_Without_Finalize'Class) is null;

   not overriding procedure Namespace_Handler (Object: Parser_Type_Without_Finalize;
                                               Namespace: RDF.Raptor.Namespaces.Namespace_Type_Without_Finalize'Class) is null;

   not overriding function Get_Locator (Parser: Parser_Type_Without_Finalize) return RDF.Raptor.Log.Locator_Type;

   not overriding procedure Parse_Abort (Parser: Parser_Type_Without_Finalize);

   not overriding procedure Parse_Chunk (Parser: Parser_Type_Without_Finalize;
                                         Buffer: String;
                                         Is_End: Boolean);

   not overriding procedure Parse_File (Parser: Parser_Type_Without_Finalize;
                                        URI: RDF.Raptor.URI.URI_Type_Without_Finalize;
                                        Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize := From_Handle(null));

   not overriding procedure Parse_Stdin (Parser: Parser_Type_Without_Finalize;
                                         Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize := From_Handle(null));

   not overriding procedure Parse_File_Stream (Parser: Parser_Type_Without_Finalize;
                                               Stream: RDF.Auxiliary.C_File_Access;
                                               Filename: String;
                                               Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize);

   not overriding procedure Parse_Iostream (Parser: Parser_Type_Without_Finalize;
                                            Stream: RDF.Raptor.IOStream.Base_Stream_Type'Class;
                                            Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize);

   not overriding procedure Parse_Start (Parser: Parser_Type_Without_Finalize; URI: RDF.Raptor.URI.URI_Type_Without_Finalize);

   not overriding procedure Parse_URI (Parser: Parser_Type_Without_Finalize;
                                       URI: RDF.Raptor.URI.URI_Type_Without_Finalize;
                                       Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize := From_Handle(null));

   not overriding procedure Parse_URI_With_Connection (Parser: Parser_Type_Without_Finalize;
                                                       URI: RDF.Raptor.URI.URI_Type_Without_Finalize;
                                                       Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize := From_Handle(null);
                                                       Connection: RDF.Raptor.WWW.Connection_Type := null);

   not overriding function Get_Graph (Parser: Parser_Type_Without_Finalize) return RDF.Raptor.URI.URI_Type;

   not overriding function Get_Description (Parser: Parser_Type_Without_Finalize) return RDF.Raptor.Syntaxes.Syntax_Description_Type;

   not overriding function Get_Name (Parser: Parser_Type_Without_Finalize) return String;

   not overriding procedure Set_Option (Parser: Parser_Type_Without_Finalize; Option: RDF.Raptor.Options.Raptor_Option; Value: String);
   not overriding procedure Set_Option (Parser: Parser_Type_Without_Finalize; Option: RDF.Raptor.Options.Raptor_Option; Value: int);

   -- TODO: Not sure if we should be able to query here whether the option is numeric
   not overriding function Get_Numeric_Option (Parser: Parser_Type_Without_Finalize; Option: RDF.Raptor.Options.Raptor_Option) return int;
   not overriding function Get_String_Option (Parser: Parser_Type_Without_Finalize; Option: RDF.Raptor.Options.Raptor_Option) return String;

   -- TODO: Stopped at raptor_parser_get_accept_header()

   -- This type can provide a small performance benefit over Parser_Type defined below.
   -- However if your main concern is reliability, not performance,
   -- you may wish use Parser_Type defined below.
   type Parser_Type is new Parser_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Object: Parser_Type; Handle: Handle_Type);

   not overriding function Create (World: RDF.Raptor.World.World_Type_Without_Finalize'Class; Name: String) return Parser_Type;

   not overriding function Create_From_Content (World: RDF.Raptor.World.World_Type_Without_Finalize'Class;
                                                URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class;
                                                Mime_Type: String_Holders.Holder;
                                                Buffer: String_Holders.Holder;
                                                Identifier: String_Holders.Holder)
                                                return Parser_Type;

end RDF.Raptor.Parser;
