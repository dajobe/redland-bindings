with RDF.Auxiliary.Limited_Handled_Record;

package RDF.Redland.Query is

   package Query_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Query_Type_Without_Finalize is new RDF.Redland.Query.Query_Handled_Record.Base_Object with null record;

   subtype Query_Handle is Query_Handled_Record.Access_Type;

   -- TODO: Stopped here

end RDF.Redland.Query;
