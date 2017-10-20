with RDF.Auxiliary.Limited_Handled_Record;

package RDF.Redland.Query_Results is

   package Query_Results_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Query_Results_Type_Without_Finalize is new RDF.Redland.Query_Results.Query_Results_Handled_Record.Base_Object with null record;

   subtype Query_Results_Handle is Query_Results_Handled_Record.Access_Type;

   -- TODO: Stopped here

   package Finalizer is new Query_Results_Handled_Record.With_Finalization(Query_Results_Type_Without_Finalize);

   type Query_Results_Type is new Finalizer.Derived with null record;

end RDF.Redland.Query_Results;
