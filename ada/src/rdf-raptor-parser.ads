with Interfaces.C; use Interfaces.C;
with RDF.Auxiliary; use RDF.Auxiliary;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Auxiliary.C_String_Holders; use RDF.Auxiliary.C_String_Holders;
with RDF.Raptor.World;
with RDF.Raptor.URI;
with RDF.Raptor.Statement;
with RDF.Raptor.Namespaces;
with RDF.Raptor.Syntaxes;
with RDF.Raptor.Log;

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

   -- TODO: Stopped at raptor_parser_parse_chunk()

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

   not overriding function Get_Description (Parser: Parser_Type) return RDF.Raptor.Syntaxes.Syntax_Description_Type;

end RDF.Raptor.Parser;
