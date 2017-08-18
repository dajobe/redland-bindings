with RDF.Rasqal.Query;

package body RDF.Rasqal.Query_Results is

   function rasqal_new_query_results2 (World: RDF.Rasqal.World.Handle_Type;
                                       Query: RDF.Rasqal.Query.Query_Handle_Type;
                                       Kind: Query_Results_Type_Enum)
                                       return Query_Results_Handle_Type
     with Import, Convention=>C;

   function New_Query_Results (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                               Query: RDF.Rasqal.Query.Query_Type_Without_Finalize;
                               Kind: Query_Results_Type_Enum)
                               return Query_Results_Type is
      use RDF.Rasqal.World, RDF.Rasqal.Query;
   begin
      return From_Non_Null_Handle(rasqal_new_query_results2(Get_Handle(World), Get_Handle(Query), Kind));
   end New_Query_Results;

end RDF.Rasqal.Query_Results;
