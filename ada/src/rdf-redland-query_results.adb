with Ada.Containers;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body RDF.Redland.Query_Results is

   function librdf_query_results_as_stream (Results: Query_Results_Handle) return Stream_Handle
     with Import, Convention=>C;

   function As_Stream (Results: Query_Results_Type_Without_Finalize) return Stream_Type is
   begin
      return From_Non_Null_Handle(librdf_query_results_as_stream(Get_Handle(Results)));
   end;

   function librdf_query_results_get_count (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   function Get_Current_Count (Results: Query_Results_Type_Without_Finalize) return Natural is
   begin
      return Natural(librdf_query_results_get_count(Get_Handle(Results)));
   end;

   function librdf_query_results_next (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   procedure Next (Results: Query_Results_Type_Without_Finalize'Class) is
   begin
      if librdf_query_results_next(Get_Handle(Results)) /= 0 then
         -- Check is done by Finished procedure, not here
         null; -- raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_query_results_finished (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   function Finished (Results: Query_Results_Type_Without_Finalize) return Boolean is
     (librdf_query_results_finished(Get_Handle(Results)) /= 0);

   function librdf_query_results_get_bindings_count (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   function Get_Bindings_Count (Results: Query_Results_Type_Without_Finalize'Class)
                                return Natural is
      Result: constant int := librdf_query_results_get_bindings_count(Get_Handle(Results));
   begin
      if Result < 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Natural(Result);
   end;

   type chars_ptr_ptr is access chars_ptr with Convention=>C;
   type chars_ptr_ptr_ptr is access all chars_ptr_ptr with Convention=>C;
   type Node_Handle_ptr is access all Node_Handle with Convention=>C;

   function librdf_query_results_get_bindings (Results: Query_Results_Handle;
                                               Names: chars_ptr_ptr_ptr;
                                               Values: Node_Handle_ptr)
                                               return int
     with Import, Convention=>C;

   function Get_Binding_Names (Results: Query_Results_Type_Without_Finalize'Class)
                               return String_List_Type is
      Count: constant Natural := Get_Bindings_Count(Results);
      type chars_ptr_ptr_array is array(1..Count) of aliased chars_ptr_ptr;
      Names: aliased chars_ptr_ptr_array;
      Code: constant int :=
        librdf_query_results_get_bindings(Get_Handle(Results), Names(1)'Unchecked_Access, null);
   begin
      if Code /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Result: String_List_Type do
         Reserve_Capacity(Result, Ada.Containers.Count_Type(Count));
         for Name of Names loop
            Append(Result, Value(Name.all));
         end loop;
      end return;
   end;

   function Get_Binding_Values (Results: Query_Results_Type_Without_Finalize'Class)
                                return Node_List_Type is
      Count: constant Natural := Get_Bindings_Count(Results);
      type Node_Handle_array is array(1..Count) of aliased Node_Handle;
      Values: aliased Node_Handle_array;
      Code: constant int :=
        librdf_query_results_get_bindings(Get_Handle(Results), null, Values(1)'Unchecked_Access);
   begin
      if Code /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Result: Node_List_Type do
         Reserve_Capacity(Result, Ada.Containers.Count_Type(Count));
         for Value of Values loop
            Append(Result, From_Non_Null_Handle(Value));
         end loop;
      end return;
   end;

   function librdf_query_results_get_binding_value (Results: Query_Results_Handle; Index: int)
                                                    return Node_Handle
     with Import, Convention=>C;

   function Get_Binding_Value (Results: Query_Results_Type_Without_Finalize'Class; Index: Natural)
                               return Node_Type is
   begin
      return From_Non_Null_Handle(librdf_query_results_get_binding_value(Get_Handle(Results), int(Index)));
   end;

   function librdf_query_results_get_binding_name (Results: Query_Results_Handle; Index: int)
                                                   return chars_ptr
     with Import, Convention=>C;

   function Get_Binding_Name (Results: Query_Results_Type_Without_Finalize'Class; Index: Natural)
                              return String is
      Ptr: constant chars_ptr := librdf_query_results_get_binding_name(Get_Handle(Results), int(Index));
   begin
      if Ptr = Null_Ptr then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Value(Ptr);
   end;

   function librdf_query_results_get_binding_value_by_name (Results: Query_Results_Handle;
                                                            Name: char_array)
                                                            return Node_Handle
     with Import, Convention=>C;

   function Get_Binding_Value_By_Name (Results: Query_Results_Type_Without_Finalize'Class; Name: String)
                                       return Node_Type is
      Handle: constant Node_Handle :=
        librdf_query_results_get_binding_value_by_name(Get_Handle(Results), To_C(Name));
   begin
      return From_Non_Null_Handle(Handle);
   end;

end RDF.Redland.Query_Results;
