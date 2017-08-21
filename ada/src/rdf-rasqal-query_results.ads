with RDF.Auxiliary;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.URI;
with RDF.Raptor.Statement;
with RDF.Raptor.IOStream;
with RDF.Rasqal.World;
with RDF.Rasqal.Literal;
limited with RDF.Rasqal.Query;

package RDF.Rasqal.Query_Results is

   package Query_Results_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype Query_Results_Handle_Type is Query_Results_Handled_Record.Access_Type;

   type Query_Results_Type_Without_Finalize is new Query_Results_Handled_Record.Base_Object with null record;

   type Query_Results_Type_Enum is (Results_Bindings,
                                    Results_Boolean,
                                    Results_Graph,
                                    Results_Syntax,
                                    Results_Unknown);

   -- function Add_Row is deliberately not implemented

   not overriding function Finished (Results: Query_Results_Type_Without_Finalize) return Boolean;

   not overriding function Get_Binding_Name (Results: Query_Results_Type_Without_Finalize;
                                             Offset: Natural)
                                             return String;

   not overriding function Get_Binding_Value (Results: Query_Results_Type_Without_Finalize;
                                              Offset: Natural)
                                              return RDF.Rasqal.Literal.Literal_Type_Without_Finalize;

   not overriding function Get_Binding_Value_By_Name (Results: Query_Results_Type_Without_Finalize;
                                                      Name: String)
                                                      return RDF.Rasqal.Literal.Literal_Type_Without_Finalize;

   -- rasqal_query_results_get_bindings() deliberately not implemented.
   -- Use iterators instead.

   not overriding function Get_Bindings_Count (Results: Query_Results_Type_Without_Finalize)
                                               return Natural; -- or Positive?

   -- TODO: subtypes for different types of query results (Query_Results_Type_Enum)
   -- Also corresponding subtypes for *_Without_Finalize

   not overriding function Get_Boolean (Results: Query_Results_Type_Without_Finalize) return Boolean;

   not overriding function Get_Current_Count (Results: Query_Results_Type_Without_Finalize) return Natural;

   not overriding function Get_Query (Results: Query_Results_Type_Without_Finalize)
                                      return RDF.Rasqal.Query.Query_Type_Without_Finalize;

   not overriding function Get_Triple (Results: Query_Results_Type_Without_Finalize)
                                       return RDF.Raptor.Statement.Statement_Type_Without_Finalize;

   -- Deliberately not implemented:
--     function Get_Row_By_Offset (Results: Query_Results_Type_Without_Finalize; Offset: Natural) return XXX;

   not overriding function Get_Type (Results: Query_Results_Type_Without_Finalize) return Query_Results_Type_Enum;

   not overriding function Is_Bindings (Results: Query_Results_Type_Without_Finalize) return Boolean;
   not overriding function Is_Boolean (Results: Query_Results_Type_Without_Finalize) return Boolean;
   not overriding function Is_Graph (Results: Query_Results_Type_Without_Finalize) return Boolean;
   not overriding function Is_Syntax (Results: Query_Results_Type_Without_Finalize) return Boolean;

   not overriding procedure Next (Results: Query_Results_Type_Without_Finalize);

   not overriding procedure Next_Triple (Results: Query_Results_Type_Without_Finalize);

   not overriding procedure Read (Stream: RDF.Raptor.IOStream.Stream_Type_Without_Finalize;
                                  Results: Query_Results_Type_Without_Finalize;
                                  Format_Name: String; -- "" for no format name
                                  Mime_Type: String; -- "" for no MIME type
                                  Format_URI: RDF.Raptor.URI.URI_Type_Without_Finalize;
                                  Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize);

   -- TODO: Stopped at rasqal_query_results_write ()

   -- TODO: Iterators for binding rows and triples (and also for binding names?)

   type Query_Results_Type is new Query_Results_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Object: Query_Results_Type; Handle: Query_Results_Handle_Type);

   not overriding function New_Query_Results (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                                              Query: RDF.Rasqal.Query.Query_Type_Without_Finalize;
                                              Kind: Query_Results_Type_Enum)
                                              return Query_Results_Type;

   not overriding function From_String (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                                        Kind: Query_Results_Type_Enum;
                                        Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize;
                                        Value: String)
                                        return Query_Results_Type;

end RDF.Rasqal.Query_Results;
