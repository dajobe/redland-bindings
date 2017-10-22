with Ada.Containers.Indefinite_Vectors;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Redland.Stream; use RDF.Redland.Stream;

package RDF.Redland.Query_Results is

   package Query_Results_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Query_Results_Type_Without_Finalize is new RDF.Redland.Query_Results.Query_Results_Handled_Record.Base_Object with null record;

   subtype Query_Results_Handle is Query_Results_Handled_Record.Access_Type;

   -- TODO: Use subtypes in arguments

   not overriding function As_Stream (Results: Query_Results_Type_Without_Finalize) return Stream_Type;

   not overriding function Get_Current_Count (Results: Query_Results_Type_Without_Finalize) return Natural;

   -- TODO: Subtype for this
   procedure Next (Results: Query_Results_Type_Without_Finalize'Class);

   -- TODO: Subtype for this
   function Finished (Results: Query_Results_Type_Without_Finalize) return Boolean;

   package String_Lists is new Ada.Containers.Indefinite_Vectors(Natural, String);
   type String_List_Type is new String_Lists.Vector with null record;

   function Get_Bindings_Count (Results: Query_Results_Type_Without_Finalize'Class)
                                return Natural; -- or shall we use Positive?

   function Get_Binding_Names (Results: Query_Results_Type_Without_Finalize'Class)
                               return String_List_Type;

   -- TODO: Stopped at librdf_query_results_get_bindings()

   package Finalizer is new Query_Results_Handled_Record.With_Finalization(Query_Results_Type_Without_Finalize);

   type Query_Results_Type is new Finalizer.Derived with null record;

end RDF.Redland.Query_Results;
