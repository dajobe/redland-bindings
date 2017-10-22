with Ada.Containers.Indefinite_Vectors;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Redland.URI; use RDF.Redland.URI;
with RDF.Redland.Node; use RDF.Redland.Node;
with RDF.Redland.Stream; use RDF.Redland.Stream;

package RDF.Redland.Query_Results is

   package Query_Results_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Query_Results_Type_Without_Finalize is new RDF.Redland.Query_Results.Query_Results_Handled_Record.Base_Object with null record;

   subtype Query_Results_Handle is Query_Results_Handled_Record.Access_Type;

   -- TODO: Use subtypes in arguments

   -- TODO: Iterators (I don't do binding names iterator, as you can use Get_Binding_Names instead?)

   overriding procedure Finalize_Handle (Object: Query_Results_Type_Without_Finalize;
                                         Handle: Query_Results_Handle);

   not overriding function As_Stream (Results: Query_Results_Type_Without_Finalize) return Stream_Type;

   not overriding function Get_Current_Count (Results: Query_Results_Type_Without_Finalize) return Natural;

   -- TODO: Subtype for this
   procedure Next (Results: Query_Results_Type_Without_Finalize'Class);

   -- TODO: Subtype for this
   function Finished (Results: Query_Results_Type_Without_Finalize) return Boolean;

   function Get_Bindings_Count (Results: Query_Results_Type_Without_Finalize'Class)
                                return Natural; -- or shall we use Positive?

   package String_Lists is new Ada.Containers.Indefinite_Vectors(Natural, String);
   type String_List_Type is new String_Lists.Vector with null record;

   function Get_Binding_Names (Results: Query_Results_Type_Without_Finalize'Class)
                               return String_List_Type;

   package Node_Lists is new Ada.Containers.Indefinite_Vectors(Natural, Node_Type);
   type Node_List_Type is new Node_Lists.Vector with null record;

   function Get_Binding_Values (Results: Query_Results_Type_Without_Finalize'Class)
                                return Node_List_Type;

   function Get_Binding_Value (Results: Query_Results_Type_Without_Finalize'Class; Index: Natural)
                               return Node_Type;

   function Get_Binding_Name (Results: Query_Results_Type_Without_Finalize'Class; Index: Natural)
                              return String;

   function Get_Binding_Value_By_Name (Results: Query_Results_Type_Without_Finalize'Class; Name: String)
                                       return Node_Type;

   not overriding
   function To_String (Results: Query_Results_Type_Without_Finalize;
                       Name: String := "";
                       Mime_Type: String := "";
                       Format_URI, Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                       return String;

   not overriding
   procedure To_File_Handle (Results: Query_Results_Type_Without_Finalize;
                             File: RDF.Auxiliary.C_File_Access;
                             Name: String := "";
                             Mime_Type: String := "";
                             Format_URI, Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)));

   -- librdf_query_results_to_file2() is wrong: http://bugs.librdf.org/mantis/view.php?id=639

   function Is_Bindings (Results: Query_Results_Type_Without_Finalize) return Boolean;
   function Is_Boolean  (Results: Query_Results_Type_Without_Finalize) return Boolean;
   function Is_Graph    (Results: Query_Results_Type_Without_Finalize) return Boolean;
   function Is_Syntax   (Results: Query_Results_Type_Without_Finalize) return Boolean;

   -- TODO: Stopped at librdf_query_results_get_boolean()

   package Finalizer is new Query_Results_Handled_Record.With_Finalization(Query_Results_Type_Without_Finalize);

   type Query_Results_Type is new Finalizer.Derived with null record;

end RDF.Redland.Query_Results;
