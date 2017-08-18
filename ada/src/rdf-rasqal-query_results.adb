with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Raptor.URI;
with RDF.Rasqal.Query;

package body RDF.Rasqal.Query_Results is

   function rasqal_query_results_finished (Results: Query_Results_Handle_Type) return int
     with Import, Convention=>C;

   function Finished (Results: Query_Results_Type_Without_Finalize) return Boolean is
     (rasqal_query_results_finished(Get_Handle(Results)) /= 0);

   function rasqal_query_results_get_binding_name (Results: Query_Results_Handle_Type;
                                                   Offset: int)
                                                   return chars_ptr
     with Import, Convention=>C;

   function Get_Binding_Name (Results: Query_Results_Type_Without_Finalize;
                              Offset: Natural)
                              return String is
      Ptr: constant chars_ptr := rasqal_query_results_get_binding_name(Get_Handle(Results), int(Offset));
   begin
      if Ptr = Null_Ptr then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Value(Ptr);
   end;

   procedure rasqal_free_query_results (Handle: Query_Results_Handle_Type)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Query_Results_Type; Handle: Query_Results_Handle_Type) is
   begin
      rasqal_free_query_results(Handle);
   end;

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
   end;

   function rasqal_new_query_results_from_string (World: RDF.Rasqal.World.Handle_Type;
                                                  Kind: Query_Results_Type_Enum;
                                                  Base_URI: RDF.Raptor.URI.Handle_Type;
                                                  Value: char_array;
                                                  Length: size_t)
                                                  return Query_Results_Handle_Type
     with Import, Convention=>C;

   function From_String (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                         Kind: Query_Results_Type_Enum;
                         Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize;
                         Value: String)
                         return Query_Results_Type is
      use RDF.Rasqal.World, RDF.Raptor.URI;
   begin
      return From_Non_Null_Handle(Rasqal_New_Query_Results_From_String(
                                  Get_Handle(World), Kind, Get_Handle(Base_URI), To_C(Value, Append_Nul=>False), Value'Length));
   end;

end RDF.Rasqal.Query_Results;
