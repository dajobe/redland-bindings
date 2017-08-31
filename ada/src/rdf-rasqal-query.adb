with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.C_String_Holders;

package body RDF.Rasqal.Query is

   function rasqal_query_add_data_graph (Query: Query_Handle_Type; Graph: RDF.Rasqal.Data_Graph.Data_Graph_Handle)
                                         return int
     with Import, Convention=>C;

   procedure Add_Data_Graph (Query: Query_Type_Without_Finalize;
                             Graph: RDF.Rasqal.Data_Graph.Data_Graph_Type_Without_Finalize) is
      use RDF.Rasqal.Data_Graph;
   begin
      if rasqal_query_add_data_graph (Get_Handle(Query), Get_Handle(Graph)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   procedure Add_Data_Graphs (Query: Query_Type_Without_Finalize; Graphs: Data_Graphs_Array) is
   begin
      for Graph of Graphs loop
         Add_Data_Graph(Query, Graph);
      end loop;
   end;

   function rasqal_query_execute (Query: Query_Handle_Type) return RDF.Rasqal.Query_Results.Query_Results_Handle_Type
     with Import, Convention=>C;

   function Execute (Query: Query_Type_Without_Finalize) return RDF.Rasqal.Query_Results.Query_Results_Type is
      use RDF.Rasqal.Query_Results;
   begin
      return From_Non_Null_Handle(rasqal_query_execute(Get_Handle(Query)));
   end;

   function rasqal_query_prepare (Query: Query_Handle_Type; Query_String: char_array; Base_URI: RDF.Raptor.URI.Handle_Type)
                                  return int
     with Import, Convention=>C;

   procedure Prepare (Query: Query_Type_Without_Finalize;
                      Query_String: String;
                      Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize := RDF.Raptor.URI.From_Handle(null)) is
      use RDF.Raptor.URI;
   begin
      if rasqal_query_prepare(Get_Handle(Query), To_C(Query_String), Get_Handle(Base_URI)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function rasqal_query_set_store_results (Query: Query_Handle_Type; Store: int) return int
     with Import, Convention=>C;

   procedure Set_Store_Results (Query: Query_Type_Without_Finalize; Store: Boolean) is
   begin
      if rasqal_query_set_store_results(Get_Handle(Query), (if Store then 1 else 0)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function rasqal_new_query (World: RDF.Rasqal.World.Handle_Type; Name, URI: chars_ptr) return Query_Handle_Type
     with Import, Convention=>C;

   function New_Query (World: RDF.Rasqal.World.World_Type; Name, URI: RDF.Auxiliary.String_Holders.Holder) return Query_Type is
      use RDF.Auxiliary.C_String_Holders;
      Name2: chars_ptr := New_String(Name);
      URI2 : chars_ptr := New_String(URI );
      use RDF.Rasqal.World;
      Result: constant Query_Handle_Type := rasqal_new_query(Get_Handle(World), Name2, URI2);
   begin
      Free(Name2);
      Free(URI2);
      return From_Non_Null_Handle(Result);
   end;

   procedure rasqal_free_query (Handle: Query_Handle_Type)
     with Import, Convention=>C;

   procedure Finalize_Handle (Query: Query_Type; Handle: Query_Handle_Type) is
   begin
      rasqal_free_query(Handle);
   end;

end RDF.Rasqal.Query;
