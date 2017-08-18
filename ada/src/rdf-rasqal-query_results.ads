with RDF.Auxiliary;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Rasqal.World;
limited with RDF.Rasqal.Query;

package RDF.Rasqal.Query_Results is

   package Query_Results_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype Query_Results_Handle_Type is Query_Results_Handled_Record.Access_Type;

   type Query_Results_Type_Without_Finalize is new Query_Results_Handled_Record.Base_Object with null record;

   type Query_Results_Type_Enum is (Results_Bindings,
                                    Results_Boolean,
                                    Results_Graph,
                                    Results_Syntax,
                                    Results_Unknow);

   type Query_Results_Type is new Query_Results_Type_Without_Finalize with null record;

   function New_Query_Results (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                               Query: RDF.Rasqal.Query.Query_Type_Without_Finalize;
                               Kind: Query_Results_Type_Enum)
                               return Query_Results_Type;

   -- TODO: Stopped at rasqal_new_query_results_from_string ()

end RDF.Rasqal.Query_Results;
