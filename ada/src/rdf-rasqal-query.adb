with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.C_String_Holders;
with RDF.Auxiliary.C_Pointers;
with RDF.Auxiliary.Convert; use RDF.Auxiliary.Convert;
with RDF.Rasqal.Memory;

package body RDF.Rasqal.Query is

   function rasqal_query_add_data_graph (Query: Query_Handle; Graph: Data_Graph_Handle)
                                         return int
     with Import, Convention=>C;

   procedure Add_Data_Graph (Query: in out Query_Type_Without_Finalize;
                             Graph: Data_Graph_Type_Without_Finalize'Class) is
   begin
      if rasqal_query_add_data_graph (Get_Handle(Query), Get_Handle(Graph)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   procedure Add_Data_Graphs (Query: in out Query_Type_Without_Finalize; Graphs: Data_Graphs_Array) is
   begin
      for Graph of Graphs loop
         Add_Data_Graph(Query, Graph);
      end loop;
   end;

   function rasqal_query_execute (Query: Query_Handle) return Query_Results_Handle
     with Import, Convention=>C;

   function Execute (Query: Query_Type_Without_Finalize) return Query_Results_Type is
   begin
      return From_Non_Null_Handle(rasqal_query_execute(Get_Handle(Query)));
   end;

   function rasqal_query_prepare (Query: Query_Handle; Query_String: char_array; Base_URI: URI_Handle)
                                  return int
     with Import, Convention=>C;

   procedure Prepare (Query: in out Query_Type_Without_Finalize;
                      Query_String: String;
                      Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null))) is
   begin
      if rasqal_query_prepare(Get_Handle(Query), To_C(Query_String), Get_Handle(Base_URI)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function rasqal_query_set_store_results (Query: Query_Handle; Store: int) return int
     with Import, Convention=>C;

   procedure Set_Store_Results (Query: in out Query_Type_Without_Finalize; Store: Boolean) is
   begin
      if rasqal_query_set_store_results(Get_Handle(Query), (if Store then 1 else 0)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   procedure rasqal_query_set_wildcard (Query: Query_Handle; Store: int)
     with Import, Convention=>C;

   procedure Set_Wildcard (Query: in out Query_Type_Without_Finalize; Store: Boolean) is
   begin
      rasqal_query_set_wildcard(Get_Handle(Query), (if Store then 1 else 0));
   end;

   function rasqal_query_write (Stream: IOStream_Handle;
                                Query: Query_Handle;
                                Format_URI, Base_URI: URI_Handle)
                                return Int
     with Import, Convention=>C;

   procedure Write_Query (Stream: Base_Stream_Type'Class;
                          Query: Query_Type_Without_Finalize;
                          Format_URI, Base_URI: URI_Type_Without_Finalize'Class) is
   begin
      if rasqal_query_write(Get_Handle(Stream), Get_Handle(Query), Get_Handle(Format_URI), Get_Handle(Base_URI)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function rasqal_query_iostream_write_escaped_counted_string (Query: Query_Handle;
                                                                Stream: IOStream_Handle;
                                                                Str: char_array;
                                                                Len: size_t)
                                                                return int
     with Import, Convention=>C;

   procedure Write_Escaped_String (Query: Query_Type_Without_Finalize;
                                   Stream: Base_Stream_Type'Class;
                                   Str: String) is
   begin
      if rasqal_query_iostream_write_escaped_counted_string(Get_Handle(Query),
                                                            Get_Handle(Stream),
                                                            To_C(Str, Append_Nul=>False),
                                                            Str'Length) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   type Size_T_P is access all size_t with Convention=>C;

   function Rasqal_Query_Escape_Counted_String (Query: Query_Handle;
                                                Str: char_array;
                                                Len: size_t;
                                                Out_Len: Size_T_P)
                                                return RDF.Auxiliary.C_Pointers.Pointer
     with Import, Convention=>C;

   function Escape_String (Query: Query_Type_Without_Finalize; Str: String)
                           return String is
      In_Str2: constant char_array := My_To_C_Without_Nul(Str);
      Out_Len: aliased size_t;
      Result: constant RDF.Auxiliary.C_Pointers.Pointer :=
        rasqal_query_escape_counted_string(Get_Handle(Query), In_Str2, Str'Length, Out_Len'Unchecked_Access);
      use RDF.Auxiliary.C_Pointers;
   begin
      if Result = null then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      declare
         Out_Str: constant String := Value_With_Possible_NULs(Result, Out_Len);
      begin
         RDF.Rasqal.Memory.rasqal_free_memory(Convert(Result));
         return Out_Str;
      end;
   end;

   function rasqal_query_set_feature (Query: Query_Handle;
                                      Feature: Feature_Type;
                                      Value: int)
                                      return int
     with Import, Convention=>C;

   procedure Set_Feature (Query: in out Query_Type_Without_Finalize; Feature: Feature_Type; Value: Natural) is
   begin
      if rasqal_query_set_feature(Get_Handle(Query), Feature, int(Value)) /= 0  then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function rasqal_query_set_feature_string (Query: Query_Handle;
                                             Feature: Feature_Type;
                                             Value: char_array)
                                             return int
     with Import, Convention=>C;

   procedure Set_Feature (Query: in out Query_Type_Without_Finalize; Feature: Feature_Type; Value: String) is
   begin
      if rasqal_query_set_feature_string(Get_Handle(Query), Feature, To_C(Value)) /= 0  then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function rasqal_query_get_feature (Query: Query_Handle; Feature: Feature_Type) return int
     with Import, Convention=>C;

   function Get_Feature (Query: Query_Type_Without_Finalize; Feature: Feature_Type) return Natural is
      Result: constant int := rasqal_query_get_feature(Get_Handle(Query), Feature);
   begin
      if Result < 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Natural(Result);
   end;

   function rasqal_query_get_feature_string (Query: Query_Handle; Feature: Feature_Type) return chars_ptr
     with Import, Convention=>C;

   function Get_Feature (Query: Query_Type_Without_Finalize; Feature: Feature_Type) return String is
      Result: constant chars_ptr := rasqal_query_get_feature_string(Get_Handle(Query), Feature);
   begin
      if Result = Null_Ptr then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      declare
         Result2: constant String := Value(Result);
      begin
         RDF.Rasqal.Memory.rasqal_free_memory(Result);
         return Result2;
      end;
   end;

   function rasqal_query_get_result_type (Query: Query_Handle) return Query_Results_Type_Enum
     with Import, Convention=>C;

   function Get_Result_Type (Query: Query_Type_Without_Finalize) return Query_Results_Type_Enum is
      Result: constant Query_Results_Type_Enum :=
        rasqal_query_get_result_type(Get_Handle(Query));
   begin
      if Result = Results_Unknown then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Result;
   end;

   function rasqal_new_query (World: Rasqal_World_Handle; Name, URI: chars_ptr) return Query_Handle
     with Import, Convention=>C;

   function New_Query (World: Rasqal_World_Type'Class; Name, URI: RDF.Auxiliary.String_Holders.Holder) return Query_Type is
      use RDF.Auxiliary.C_String_Holders;
      Name2: chars_ptr := New_String(Name);
      URI2 : chars_ptr := New_String(URI );
      Result: constant Query_Handle := rasqal_new_query(Get_Handle(World), Name2, URI2);
   begin
      Free(Name2);
      Free(URI2);
      return From_Non_Null_Handle(Result);
   end;

   procedure rasqal_free_query (Handle: Query_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Query: Query_Type; Handle: Query_Handle) is
   begin
      rasqal_free_query(Handle);
   end;

end RDF.Rasqal.Query;
