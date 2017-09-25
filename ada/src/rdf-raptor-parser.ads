with Interfaces.C; use Interfaces.C;
-- with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary; use RDF.Auxiliary;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Auxiliary.C_String_Holders; use RDF.Auxiliary.C_String_Holders;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.IOStream; use RDF.Raptor.IOStream;
with RDF.Raptor.Statement; use RDF.Raptor.Statement;
with RDF.Raptor.Namespace; use RDF.Raptor.Namespace;
with RDF.Raptor.Syntaxes; use RDF.Raptor.Syntaxes;
with RDF.Raptor.Log; use RDF.Raptor.Log;
with RDF.Raptor.WWW; use RDF.Raptor.WWW;
with RDF.Raptor.Options; use RDF.Raptor.Options;

package RDF.Raptor.Parser is

   package Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype Parser_Handle is Handled_Record.Access_Type;

   type Parser_Type_Without_Finalize is new Handled_Record.Base_Object with null record;

   type Graph_Mark_Flags is (Graph_Mark_Start, Graph_Mark_Declared);
   for Graph_Mark_Flags'Size use int'Size; -- hack
   for Graph_Mark_Flags use (Graph_Mark_Start=>1, Graph_Mark_Declared=>2);

   -- You can call this function or initialize only callbacks you need (below).
   not overriding procedure Initialize_All_Callbacks (Parser: Parser_Type_Without_Finalize);

   not overriding procedure Initialize_Graph_Mark_Handler (Object: Parser_Type_Without_Finalize);
   not overriding procedure Initialize_Statement_Handler  (Object: Parser_Type_Without_Finalize);
   not overriding procedure Initialize_Namespace_Handler  (Object: Parser_Type_Without_Finalize);
   not overriding procedure Initialize_URI_Filter         (Object: Parser_Type_Without_Finalize);

   not overriding procedure Graph_Mark_Handler (Object: Parser_Type_Without_Finalize;
                                                URI: URI_Type_Without_Finalize'Class;
                                                Flags: Graph_Mark_Flags) is null;

   not overriding procedure Statement_Handler (Object: Parser_Type_Without_Finalize;
                                               Statement: Statement_Type_Without_Finalize'Class) is null;

   not overriding procedure Namespace_Handler (Object: Parser_Type_Without_Finalize;
                                               Namespace: Namespace_Type_Without_Finalize'Class) is null;

   not overriding function URI_Filter (Object: Parser_Type_Without_Finalize;
                                       URI: URI_Type_Without_Finalize'Class) return Boolean is (True);

   not overriding function Get_Locator (Parser: Parser_Type_Without_Finalize) return Locator_Type;

   not overriding procedure Parse_Abort (Parser: Parser_Type_Without_Finalize);

   not overriding procedure Parse_Chunk (Parser: Parser_Type_Without_Finalize;
                                         Buffer: String;
                                         Is_End: Boolean);

   not overriding procedure Parse_File (Parser: Parser_Type_Without_Finalize;
                                        URI: URI_Type_Without_Finalize;
                                        Base_URI: URI_Type_Without_Finalize := From_Handle(null));

   not overriding procedure Parse_Stdin (Parser: Parser_Type_Without_Finalize;
                                         Base_URI: URI_Type_Without_Finalize := From_Handle(null));

   not overriding procedure Parse_File_Stream (Parser: Parser_Type_Without_Finalize;
                                               Stream: RDF.Auxiliary.C_File_Access;
                                               Filename: String;
                                               Base_URI: URI_Type_Without_Finalize);

   not overriding procedure Parse_Iostream (Parser: Parser_Type_Without_Finalize;
                                            Stream: Base_Stream_Type'Class;
                                            Base_URI: URI_Type_Without_Finalize);

   not overriding procedure Parse_Start (Parser: Parser_Type_Without_Finalize; URI: URI_Type_Without_Finalize);

   not overriding procedure Parse_URI (Parser: Parser_Type_Without_Finalize;
                                       URI: URI_Type_Without_Finalize;
                                       Base_URI: URI_Type_Without_Finalize := From_Handle(null));

   not overriding procedure Parse_URI_With_Connection (Parser: Parser_Type_Without_Finalize;
                                                       URI: URI_Type_Without_Finalize;
                                                       Base_URI: URI_Type_Without_Finalize := From_Handle(null);
                                                       Connection: Connection_Type := null);

   not overriding function Get_Graph (Parser: Parser_Type_Without_Finalize) return URI_Type;

   not overriding function Get_Description (Parser: Parser_Type_Without_Finalize) return Raptor_Syntax_Description_Type;

   not overriding function Get_Name (Parser: Parser_Type_Without_Finalize) return String;

   not overriding procedure Set_Option (Parser: Parser_Type_Without_Finalize; Option: Raptor_Option; Value: String);
   not overriding procedure Set_Option (Parser: Parser_Type_Without_Finalize; Option: Raptor_Option; Value: int);

   -- Not sure if we should be able to query here whether the option is numeric
   not overriding function Get_Numeric_Option (Parser: Parser_Type_Without_Finalize; Option: Raptor_Option) return Natural;
   not overriding function Get_String_Option (Parser: Parser_Type_Without_Finalize; Option: Raptor_Option) return String;

   not overriding function Get_Accept_Header (Parser: Parser_Type_Without_Finalize) return String;

   not overriding function Get_World (Parser: Parser_Type_Without_Finalize) return Raptor_World_Type_Without_Finalize;

   -- This type can provide a small performance benefit over Parser_Type defined below.
   -- However if your main concern is reliability, not performance,
   -- you may wish use Parser_Type defined below.
   type Parser_Type is new Parser_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Object: Parser_Type; Handle: Parser_Handle);

   not overriding function Create (World: Raptor_World_Type_Without_Finalize'Class; Name: String) return Parser_Type;

   not overriding function Create_From_Content (World: Raptor_World_Type_Without_Finalize'Class;
                                                URI: URI_Type_Without_Finalize'Class;
                                                Mime_Type: String_Holders.Holder;
                                                Buffer: String_Holders.Holder;
                                                Identifier: String_Holders.Holder)
                                                return Parser_Type;

end RDF.Raptor.Parser;
