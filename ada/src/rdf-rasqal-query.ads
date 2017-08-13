with RDF.Auxiliary;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Rasqal.World;

package RDF.Rasqal.Query is

   -- This API is partial, as I consider many C functions as internals.

   package Query_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype Query_Handle_Type is Query_Handled_Record.Access_Type;

   type Query_Type_Without_Finalize is new Query_Handled_Record.Base_Object with null record;

   -- TODO: Stopped at rasqal_query_add_data_graph()

   type Query_Type is new Query_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Query: Query_Type; Handle: Query_Handle_Type);

   not overriding function New_Query (World: RDF.Rasqal.World.World_Type; Name, URI: RDF.Auxiliary.String_Holders.Holder) return Query_Type;

end RDF.Rasqal.Query;
