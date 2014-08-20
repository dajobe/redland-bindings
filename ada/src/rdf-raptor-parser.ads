with Interfaces.C; use Interfaces.C;
with RDF.Auxilary; use RDF.Auxilary;
with RDF.Auxilary.Limited_Handled_Record;
with RDF.Auxilary.C_String_Holders; use RDF.Auxilary.C_String_Holders;
with RDF.Raptor.World;
with RDF.Raptor.URI;
with RDF.Raptor.Statement;
with RDF.Raptor.Namespaces;

package RDF.Raptor.Parser is

   package Handled_Record is new RDF.Auxilary.Limited_Handled_Record(RDF.Auxilary.Dummy_Record);

   subtype Handle_Type is Handled_Record.Access_Type;

   type Parser_Type_Without_Finalize is new Handled_Record.Base_Object with null record;

   -- You can call this function or initialize only callbacks you need (below).
   not overriding procedure Initialize_All_Callbacks (Parser: Parser_Type_Without_Finalize);

   not overriding procedure Initialize_Graph_Mark_Handler (Object: Parser_Type_Without_Finalize);
   not overriding procedure Initialize_Statement_Handler  (Object: Parser_Type_Without_Finalize);
   not overriding procedure Initialize_Namespace_Handler  (Object: Parser_Type_Without_Finalize);

   not overriding procedure Graph_Mark_Handler (Object: Parser_Type_Without_Finalize;
                                                URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class;
                                                Flags: Int) is null;

   not overriding procedure Statement_Handler (Object: Parser_Type_Without_Finalize;
                                               Statement: RDF.Raptor.Statement.Statement_Type_Without_Finalize'Class) is null;

   not overriding procedure Namespace_Handler (Object: Parser_Type_Without_Finalize;
                                               Namespace: RDF.Raptor.Namespaces.Namespace_Type_Without_Finalize'Class) is null;

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

   -- TODO: Stopped at enum raptor_graph_mark_flags and then raptor_parser_get_description

end RDF.Raptor.Parser;
