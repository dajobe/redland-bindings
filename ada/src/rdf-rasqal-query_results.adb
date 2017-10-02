with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Rasqal.Query; use RDF.Rasqal.Query;

package body RDF.Rasqal.Query_Results is

   function rasqal_query_results_finished (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   function Finished (Results: Query_Results_Type_Without_Finalize) return Boolean is
     (rasqal_query_results_finished(Get_Handle(Results)) /= 0);

   function rasqal_query_results_get_binding_name (Results: Query_Results_Handle;
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

   function rasqal_query_results_get_binding_value (Results: Query_Results_Handle;
                                                    Offset: int)
                                                    return Literal_Handle
     with Import, Convention=>C;

   function Get_Binding_Value (Results: Query_Results_Type_Without_Finalize;
                               Offset: Natural)
                               return Literal_Type_Without_Finalize is
   begin
      return From_Non_Null_Handle(rasqal_query_results_get_binding_value(Get_Handle(Results), int(Offset)));
   end;

   function rasqal_query_results_get_binding_value_by_name (Results: Query_Results_Handle;
                                                            Name: char_array)
                                                            return Literal_Handle
     with Import, Convention=>C;

   function Get_Binding_Value_By_Name (Results: Query_Results_Type_Without_Finalize;
                                       Name: String)
                                       return Literal_Type_Without_Finalize is
   begin
      return From_Non_Null_Handle(rasqal_query_results_get_binding_value_by_name(Get_Handle(Results), To_C(Name)));
   end;

   function rasqal_query_results_get_bindings_count (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   function Get_Bindings_Count (Results: Query_Results_Type_Without_Finalize) return Natural is
      Count: constant int := rasqal_query_results_get_bindings_count(Get_Handle(Results));
   begin
      if Count < 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Positive(Count);
   end;

   function rasqal_query_results_get_boolean (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   function Get_Boolean (Results: Query_Results_Type_Without_Finalize) return Boolean is
      Value: constant int := rasqal_query_results_get_boolean(Get_Handle(Results));
   begin
      if Value < 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Value /= 0;
   end;

   function rasqal_query_results_get_count (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   function Get_Current_Count (Results: Query_Results_Type_Without_Finalize) return Natural is
      Value: constant int := rasqal_query_results_get_count(Get_Handle(Results));
   begin
      if Value < 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Natural(Value);
   end;

   function rasqal_query_results_get_query (Results: Query_Results_Handle)
                                            return RDF.Rasqal.Query.Query_Handle
     with Import, Convention=>C;

   function Get_Query (Results: Query_Results_Type_Without_Finalize)
                       return RDF.Rasqal.Query.Query_Type_Without_Finalize is
   begin
      -- TODO: From_Non_Null_Handle?
      return From_Handle(rasqal_query_results_get_query(Get_Handle(Results)));
   end;

   function rasqal_query_results_get_triple (Results: Query_Results_Handle)
                                             return Statement_Handle
     with Import, Convention=>C;

   function Get_Triple (Results: Query_Results_Type_Without_Finalize)
                        return Statement_Type_Without_Finalize is
   begin
      return From_Non_Null_Handle(rasqal_query_results_get_triple(Get_Handle(Results)));
   end;

   function rasqal_query_results_get_type (Results: Query_Results_Handle) return Query_Results_Type_Enum
     with Import, Convention=>C;

   function Get_Type (Results: Query_Results_Type_Without_Finalize) return Query_Results_Type_Enum is
   begin
      return rasqal_query_results_get_type(Get_Handle(Results));
   end;

   -------------------------------------

   function rasqal_query_results_is_bindings (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   function Is_Bindings (Results: Query_Results_Type_Without_Finalize) return Boolean is
     (rasqal_query_results_is_bindings(Get_Handle(Results)) /= 0);

   function rasqal_query_results_is_boolean (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   function Is_Boolean (Results: Query_Results_Type_Without_Finalize) return Boolean is
     (rasqal_query_results_is_boolean(Get_Handle(Results)) /= 0);

   function rasqal_query_results_is_graph (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   function Is_Graph (Results: Query_Results_Type_Without_Finalize) return Boolean is
     (rasqal_query_results_is_graph(Get_Handle(Results)) /= 0);

   function rasqal_query_results_is_syntax (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   function Is_Syntax (Results: Query_Results_Type_Without_Finalize) return Boolean is
     (rasqal_query_results_is_syntax(Get_Handle(Results)) /= 0);

   -------------------------------------

   function rasqal_query_results_next (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   procedure Next (Results: Query_Results_Type_Without_Finalize) is
   begin
      if rasqal_query_results_next(Get_Handle(Results)) /= 0 then
         -- Check is done by Finished procedure, not here
         null; -- raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function rasqal_query_results_next_triple (Results: Query_Results_Handle) return int
     with Import, Convention=>C;

   procedure Next_Triple (Results: Query_Results_Type_Without_Finalize) is
   begin
      if rasqal_query_results_next_triple(Get_Handle(Results)) /= 0 then
         -- Check is done by Finished procedure, not here
         null; -- raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function rasqal_query_results_read (Stream: IOStream_Handle;
                                       Results: Query_Results_Handle;
                                       Name, Mime_Type: chars_ptr;
                                       Format_URI, Base_URI: URI_Handle)
                                       return Int
     with Import, Convention=>C;

   procedure Read (Stream: Base_Stream_Type'Class;
                   Results: Query_Results_Type_Without_Finalize;
                   Format_Name: String; -- "" for no format name
                   Mime_Type: String; -- "" for no MIME type
                   Format_URI: URI_Type_Without_Finalize'Class;
                   Base_URI: URI_Type_Without_Finalize'Class) is
      Format_Name2: aliased char_array := To_C(Format_Name);
      Mime_Type2  : aliased char_array := To_C(Mime_Type);
   begin
      if rasqal_query_results_read(Get_Handle(Stream),
                                   Get_Handle(Results),
                                   (if Format_Name = "" then Null_Ptr else To_Chars_Ptr(Format_Name2'Unchecked_Access)),
                                   (if Mime_Type = "" then Null_Ptr else To_Chars_Ptr(Mime_Type2'Unchecked_Access)),
                                   Get_Handle(Format_URI),
                                   Get_Handle(Base_URI)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function rasqal_query_results_write (Stream: IOStream_Handle;
                                        Results: Query_Results_Handle;
                                        Name, Mime_Type: Chars_Ptr;
                                        Format_URI, Base_URI: URI_Handle)
                                        return Int
     with Import, Convention=>C;

   procedure Write (Stream: Base_Stream_Type'Class;
                    Results: Query_Results_Type_Without_Finalize;
                    Format_Name: String; -- "" for no format name
                    Mime_Type: String; -- "" for no MIME type
                    Format_URI: URI_Type_Without_Finalize'Class;
                    Base_URI: URI_Type_Without_Finalize'Class) is
      Format_Name2: aliased char_array := To_C(Format_Name);
      Mime_Type2  : aliased char_array := To_C(Mime_Type);
   begin
      if rasqal_query_results_write(Get_Handle(Stream),
                                    Get_Handle(Results),
                                    (if Format_Name = "" then Null_Ptr else To_Chars_Ptr(Format_Name2'Unchecked_Access)),
                                    (if Mime_Type = "" then Null_Ptr else To_Chars_Ptr(Mime_Type2'Unchecked_Access)),
                                    Get_Handle(Format_URI),
                                    Get_Handle(Base_URI)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function rasqal_query_results_type_label (Kind: Query_Results_Type_Enum) return Chars_Ptr
     with Import, Convention=>C;

   function Type_Label (Kind: Query_Results_Type_Enum) return String is
      Ptr: constant chars_ptr := rasqal_query_results_type_label(Kind);
   begin
      if Ptr = Null_Ptr then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Value(Ptr);
   end;

   function rasqal_query_results_rewind (Results: Query_Results_Handle) return Int
     with Import, Convention=>C;

   procedure Rewind (Results: in out Query_Results_Type_Without_Finalize) is
   begin
      if rasqal_query_results_rewind(Get_Handle(Results)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   procedure rasqal_free_query_results (Handle: Query_Results_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Query_Results_Type; Handle: Query_Results_Handle) is
   begin
      rasqal_free_query_results(Handle);
   end;

   -- TODO: Not supported as of Rasqal 0.9.32
--     function rasqal_new_query_results2 (World: Rasqal_World_Handle;
--                                         Query: RDF.Rasqal.Query.Query_Handle;
--                                         Kind: Query_Results_Type_Enum)
--                                         return Query_Results_Handle
--       with Import, Convention=>C;
--
--     function New_Query_Results (World: Rasqal_World_Type_Without_Finalize;
--                                 Query: RDF.Rasqal.Query.Query_Type_Without_Finalize;
--                                 Kind: Query_Results_Type_Enum)
--                                 return Query_Results_Type is
--        use RDF.Rasqal.World, RDF.Rasqal.Query;
--     begin
--        return From_Non_Null_Handle(rasqal_new_query_results2(Get_Handle(World), Get_Handle(Query), Kind));
--     end;

   function rasqal_new_query_results (World: Rasqal_World_Handle;
                                      Query: RDF.Rasqal.Query.Query_Handle;
                                      Kind: Query_Results_Type_Enum;
                                      Vars_Table: RDF.Auxiliary.Dummy_Record_Access)
                                      return Query_Results_Handle
     with Import, Convention=>C;

   function New_Query_Results (World: Rasqal_World_Type_Without_Finalize'Class;
                               Query: RDF.Rasqal.Query.Query_Type_Without_Finalize;
                               Kind: Query_Results_Type_Enum)
                               return Query_Results_Type is
   begin
      return From_Non_Null_Handle(rasqal_new_query_results(Get_Handle(World), Get_Handle(Query), Kind, null));
   end;

--     function rasqal_new_query_results_from_string (World: Rasqal_World_Handle;
--                                                    Kind: Query_Results_Type_Enum;
--                                                    Base_URI: Handle_Type;
--                                                    Value: char_array;
--                                                    Length: size_t)
--                                                    return Query_Results_Handle
--       with Import, Convention=>C;
--
--     function From_String (World: Rasqal_World_Type_Without_Finalize;
--                           Kind: Query_Results_Type_Enum;
--                           Base_URI: URI_Type_Without_Finalize;
--                           Value: String)
--                           return Query_Results_Type is
--        use RDF.Rasqal.World, RDF.Raptor.URI;
--     begin
--        return From_Non_Null_Handle(Rasqal_New_Query_Results_From_String(
--                                    Get_Handle(World), Kind, Get_Handle(Base_URI), My_To_C_Without_Nul(Value), Value'Length));
--     end;

   function Has_Element (Position: Cursor) return Boolean is
     (Not_Finished(Position.all));

   function Create_Bindings_Iterator (Results: in out Query_Results_Type_Without_Finalize'Class)
                                      return Bindings_Iterator is
     (Ref=>Results'Unchecked_Access);

   function First (Object: Bindings_Iterator) return Cursor is
      (Object.Ref);

   function Next (Object: Bindings_Iterator; Position: Cursor) return Cursor is
   begin
      Next(Position.all);
      return Position;
   end;

   function Get_Binding_Value (Position: Cursor;
                               Offset: Natural)
                               return Literal_Type_Without_Finalize is
      (Get_Binding_Value(Position.all, Offset));

   function Get_Binding_Value_By_Name (Position: Cursor;
                                       Name: String)
                                       return Literal_Type_Without_Finalize is
      (Get_Binding_Value_By_Name(Position.all, Name));

   function Create_Triples_Iterator (Results: in out Query_Results_Type_Without_Finalize'Class)
                                     return Bindings_Iterator is
     (Ref=>Results'Unchecked_Access);

   function First (Object: Triples_Iterator) return Cursor is
      (Object.Ref);

   function Next (Object: Triples_Iterator; Position: Cursor) return Cursor is
   begin
      Next_Triple(Position.all);
      return Position;
   end;

   function Get_Triple (Position: Cursor) return Statement_Type_Without_Finalize is
      (Get_Triple(Position.all));

   -----------------

   function Has_Element (Position: Variables_Cursor) return Boolean is
      (Position.Count < Get_Bindings_Count(Position.Ref.all));

   function Create_Variables_Iterator (Results: Query_Results_Type_Without_Finalize'Class)
                                       return Variables_Iterator is
     (Ref=>Results'Unchecked_Access);

   function First (Object: Variables_Iterator) return Variables_Cursor is
     (Ref=>Object.Ref, Count=>0);

   function Next (Object: Variables_Iterator; Position: Variables_Cursor) return Variables_Cursor is
     (Position with delta Count=>Position.Count);

   function Get_Name (Position: Variables_Cursor) return String is
     (Get_Binding_Name(Position.Ref.all, Position.Count));

end RDF.Rasqal.Query_Results;
