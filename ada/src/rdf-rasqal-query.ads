with RDF.Auxiliary;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.URI;
with RDF.Raptor.IOStream;
with RDF.Rasqal.World;
with RDF.Rasqal.Features;
with RDF.Rasqal.Data_Graph;
with RDF.Rasqal.Query_Results;

package RDF.Rasqal.Query is

   -- This API is partial, as I consider many C functions as internals.

   package Query_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype Query_Handle_Type is Query_Handled_Record.Access_Type;

   type Query_Type_Without_Finalize is new Query_Handled_Record.Base_Object with null record;

   not overriding procedure Add_Data_Graph (Query: Query_Type_Without_Finalize;
                                            Graph: RDF.Rasqal.Data_Graph.Data_Graph_Type_Without_Finalize);

   type Data_Graphs_Array is array(Integer range <>) of RDF.Rasqal.Data_Graph.Data_Graph_Type_Without_Finalize;

   not overriding procedure Add_Data_Graphs (Query: Query_Type_Without_Finalize; Graphs: Data_Graphs_Array);

   not overriding function Execute (Query: Query_Type_Without_Finalize) return RDF.Rasqal.Query_Results.Query_Results_Type;

   not overriding procedure Prepare (Query: Query_Type_Without_Finalize;
                                     Query_String: String;
                                     Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize := RDF.Raptor.URI.From_Handle(null));

   not overriding procedure Set_Store_Results (Query: Query_Type_Without_Finalize; Store: Boolean);

   not overriding procedure Set_Wildcard (Query: Query_Type_Without_Finalize; Store: Boolean);

   not overriding procedure Write_Query (Stream: RDF.Raptor.IOStream.Stream_Type_Without_Finalize;
                                         Query: Query_Type_Without_Finalize;
                                         Format_URI, Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize);

   -- Is it really useful? Maybe remove from public API?
   not overriding procedure Write_Escaped_String (Query: Query_Type_Without_Finalize;
                                                  Stream: RDF.Raptor.IOStream.Stream_Type_Without_Finalize;
                                                  Str: String);

   not overriding function Escape_String (Query: Query_Type_Without_Finalize; Str: String)
                                          return String;

   not overriding procedure Set_Feature (Query: Query_Type_Without_Finalize; Feature: RDF.Rasqal.Features.Feature_Type; Value: Natural);
   not overriding procedure Set_Feature (Query: Query_Type_Without_Finalize; Feature: RDF.Rasqal.Features.Feature_Type; Value: String);

   not overriding function Get_Feature (Query: Query_Type_Without_Finalize; Feature: RDF.Rasqal.Features.Feature_Type)
                                        return Natural;

   -- Currently it raises an exception, if there is no such feature. Is it correct?
   not overriding function Get_Feature (Query: Query_Type_Without_Finalize; Feature: RDF.Rasqal.Features.Feature_Type)
                                        return String;

   not overriding function Get_Result_Type (Query: Query_Type_Without_Finalize)
                                            return RDF.Rasqal.Query_Results.Query_Results_Type_Enum;

   -- Deliberately not implemented
--     function Get_Update_Operation;
--     function Get_Update_Operations_Sequence;

   type Query_Type is new Query_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Query: Query_Type; Handle: Query_Handle_Type);

   not overriding function New_Query (World: RDF.Rasqal.World.World_Type; Name, URI: RDF.Auxiliary.String_Holders.Holder) return Query_Type;

end RDF.Rasqal.Query;
