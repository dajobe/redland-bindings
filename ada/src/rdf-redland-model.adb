with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.Convert; use RDF.Auxiliary.Convert;
with RDF.Redland.Query; use RDF.Redland.Query;
with RDF.Redland.Query_Results; use RDF.Redland.Query_Results;

package body RDF.Redland.Model is

   type String_P_Type is access all chars_ptr with Convention=>C;

   function librdf_model_enumerate (World: Redland_World_Handle;
                                    Counter: unsigned;
                                    Name, Label: String_P_Type)
                                    return int
     with Import, Convention=>C;

   function Enumerate_Models (World: Redland_World_Type_Without_Finalize'Class;
                              Counter: unsigned)
                              return Model_Info_Holders.Holder is
      Name, Label: aliased chars_ptr;
      Result: constant int :=
        librdf_model_enumerate(Get_Handle(World), Counter, Name'Unchecked_Access, Label'Unchecked_Access);
      use Model_Info_Holders;
   begin
      if Result /= 0 then
         return Empty_Holder;
      end if;
      declare
         Name2 : constant String := Value(Name );
         Label2: constant String := Value(Label);
      begin
         return To_Holder((Name_Length  => Name2 'Length,
                           Label_Length => Label2'Length,
                           Name => Name2, Label => Label2));
      end;
   end;

   function Has_Element (Position: Enumerate_Models_Cursor) return Boolean is
   begin
      return librdf_model_enumerate(Position.World, Position.Counter, null, null) = 0;
   end;

   function First (Object: Enumerate_Models_Iterator) return Enumerate_Models_Cursor is
   begin
      return (World => Object.World, Counter => 0);
   end;

   function Next (Object: Enumerate_Models_Iterator; Position: Enumerate_Models_Cursor)
                  return Enumerate_Models_Cursor is
   begin
      return (World => Position.World, Counter => Position.Counter + 1);
   end;

   function librdf_new_model (World: Redland_World_Handle; Storage: Storage_Handle; Options: char_array)
                              return Model_Handle
     with Import, Convention=>C;

   function Create (World: Redland_World_Type_Without_Finalize'Class;
                    Storage: Storage_Type_Without_Finalize'Class;
                    Options: String := "")
                    return Model_Type is
      Handle: constant Model_Handle :=
        librdf_new_model(Get_Handle(World), Get_Handle(Storage), To_C(Options));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_new_model_from_model (Model: Model_Handle) return Model_Handle
     with Import, Convention=>C;

   function Copy (Model: Model_Type_Without_Finalize'Class) return Model_Type is
   begin
      return From_Non_Null_Handle(librdf_new_model_from_model(Get_Handle(Model)));
   end;

   procedure librdf_free_model (Handle: Model_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Model_Type_Without_Finalize; Handle: Model_Handle) is
   begin
      librdf_free_model(Handle);
   end;

   function librdf_model_size (Model: Model_Handle) return int
     with Import, Convention=>C;

   function Size_Without_Exception (Model: Model_Type_Without_Finalize)
                                    return Integer is
   begin
      return Integer(librdf_model_size(Get_Handle(Model)));
   end;

   function Size (Model: Model_Type_Without_Finalize) return Natural is
      Result: constant int := librdf_model_size(Get_Handle(Model));
   begin
      if Result < 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Natural(Result);
   end;

   function librdf_model_add (Model: Model_Handle; Subject, Predicate, Object: Node_Handle) return int
     with Import, Convention=>C;

   procedure Add (Model: in out Model_Type_Without_Finalize;
                  Subject, Predicate, Object: Node_Type_Without_Finalize'Class) is
   begin
      if librdf_model_add(Get_Handle(Model), Get_Handle(Subject), Get_Handle(Predicate), Get_Handle(Object)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_model_add_string_literal_statement (Model: Model_Handle;
                                                       Subject, Predicate: Node_Handle;
                                                       Literal: char_array;
                                                       Language: char_array;
                                                       Is_XML: int)
                                                       return int
     with Import, Convention=>C;

   procedure Add_String_Literal_Statement (Model: Model_Type_Without_Finalize;
                                           Subject, Predicate: Node_Type_Without_Finalize'Class;
                                           Literal: String;
                                           Language: String;
                                           Is_XML: Boolean := False) is
   begin
      if librdf_model_add_string_literal_statement(Get_Handle(Model),
                                                   Get_Handle(Subject),
                                                   Get_Handle(Predicate),
                                                   To_C(Literal),
                                                   To_C(Language),
                                                   (if Is_XML then 1 else 0)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_model_add_typed_literal_statement (Model: Model_Handle;
                                                      Subject, Predicate: Node_Handle;
                                                      Literal: char_array;
                                                      Language: char_array;
                                                      Datatype: URI_Handle)
                                                      return int
     with Import, Convention=>C;

   procedure Add_Typed_Literal_Statement (Model: Model_Type_Without_Finalize;
                                          Subject, Predicate: Node_Type_Without_Finalize'Class;
                                          Literal: String;
                                          Language: String;
                                          Datatype: URI_Type_Without_Finalize'Class) is
   begin
      if librdf_model_add_typed_literal_statement(Get_Handle(Model),
                                                  Get_Handle(Subject),
                                                  Get_Handle(Predicate),
                                                  To_C(Literal),
                                                  To_C(Language),
                                                  Get_Handle(Datatype)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_model_context_add_statement (Model: Model_Handle;
                                                Context: Node_Handle;
                                                Statement: Statement_Handle)
                                                return int
     with Import, Convention=>C;

   procedure Add (Model: in out Model_Type_Without_Finalize;
                  Statement: Statement_Type_Without_Finalize'Class;
                  Context: Node_Type_Without_Finalize'Class := Node_Type_Without_Finalize'(From_Handle(null))) is
   begin
      if librdf_model_context_add_statement(Get_Handle(Model),
                                            Get_Handle(Context),
                                            Get_Handle(Statement)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_model_context_add_statements (Model_Type: Model_Handle;
                                                 Context: Node_Handle;
                                                 Statements: Stream_Handle)
                                                 return int
     with Import, Convention=>C;

   procedure Add (Model: in out Model_Type_Without_Finalize;
                  Statements: Stream_Type_Without_Finalize'Class;
                  Context: Node_Type_Without_Finalize'Class := Node_Type_Without_Finalize'(From_Handle(null))) is
   begin
      if librdf_model_context_add_statements(Get_Handle(Model),
                                             Get_Handle(Context),
                                             Get_Handle(Statements)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_model_context_remove_statement (Model: Model_Handle;
                                                   Context: Node_Handle;
                                                   Statement: Statement_Handle)
                                                   return int
     with Import, Convention=>C;

   procedure Remove (Model: in out Model_Type_Without_Finalize;
                     Statement: Statement_Type_Without_Finalize'Class;
                     Context: Node_Type_Without_Finalize'Class := Node_Type_Without_Finalize'(From_Handle(null))) is
   begin
      if librdf_model_context_remove_statement(Get_Handle(Model),
                                               Get_Handle(Context),
                                               Get_Handle(Statement)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_model_context_remove_statements (Model_Type: Model_Handle;
                                                    Context: Node_Handle;
                                                    Statements: Stream_Handle)
                                                    return int
     with Import, Convention=>C;

   procedure Remove (Model: in out Model_Type_Without_Finalize;
                     Statements: Stream_Type_Without_Finalize'Class;
                     Context: Node_Type_Without_Finalize'Class := Node_Type_Without_Finalize'(From_Handle(null))) is
   begin
      if librdf_model_context_remove_statements(Get_Handle(Model),
                                                Get_Handle(Context),
                                                Get_Handle(Statements)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_model_remove_statement (Model: Model_Handle; Statement: Statement_Handle) return int
     with Import, Convention=>C;

   procedure Remove (Model: Model_Type_Without_Finalize; Statement: Statement_Type_Without_Finalize'Class) is
   begin
      if librdf_model_remove_statement(Get_Handle(Model), Get_Handle(Statement)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_model_contains_statement (Model: Model_Handle; Statement: Statement_Handle) return int
     with Import, Convention=>C;

   function Contains (Model: Model_Type_Without_Finalize;
                      Statement: Statement_Type_Without_Finalize'Class)
                      return Boolean is
      Result: constant int :=
        librdf_model_contains_statement(Get_Handle(Model), Get_Handle(Statement));
   begin
      if Result > 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Result /= 0;
   end;

   function librdf_model_has_arc_in (Model: Model_Handle; Node, Property: Node_Handle) return int
     with Import, Convention=>C;

   not overriding function Has_Arc_In (Model: Model_Type_Without_Finalize;
                                       Node, Property: Node_Type_Without_Finalize'Class)
                                       return Boolean is
   begin
      return librdf_model_has_arc_in(Get_Handle(Model), Get_Handle(Node), Get_Handle(Property)) /= 0;
   end;

   function librdf_model_has_arc_out (Model: Model_Handle; Node, Property: Node_Handle) return int
     with Import, Convention=>C;

   not overriding function Has_Arc_Out (Model: Model_Type_Without_Finalize;
                                        Node, Property: Node_Type_Without_Finalize'Class)
                                        return Boolean is
   begin
      return librdf_model_has_arc_out(Get_Handle(Model), Get_Handle(Node), Get_Handle(Property)) /= 0;
   end;

   function librdf_model_as_stream (Model: Model_Handle) return Stream_Handle
     with Import, Convention=>C;

   function As_Stream (Model: Model_Type_Without_Finalize) return Stream_Type is
   begin
      return From_Non_Null_Handle(librdf_model_as_stream(Get_Handle(Model)));
   end;

   function librdf_model_find_statements (Model: Model_Handle; Statement: Statement_Handle) return Stream_Handle
     with Import, Convention=>C;

   function Find (Model: Model_Type_Without_Finalize;
                  Statement: Statement_Type_Without_Finalize'Class)
                  return Stream_Type is
   begin
      return From_Non_Null_Handle(librdf_model_find_statements(Get_Handle(Model), Get_Handle(Statement)));
   end;

   function librdf_model_get_sources (Model: Model_Handle; Arc, Target: Node_Handle)
                                      return Node_Iterator_Handle
     with Import, Convention=>C;

   function Get_Sources (Model: Model_Type_Without_Finalize;
                         Arc, Target: Node_Type_Without_Finalize'Class)
                         return Node_Iterator_Type is
      Handle: constant Node_Iterator_Handle :=
        librdf_model_get_sources(Get_Handle(Model), Get_Handle(Arc), Get_Handle(Target));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_model_get_arcs (Model: Model_Handle; Source, Target: Node_Handle)
                                   return Node_Iterator_Handle
     with Import, Convention=>C;

   function Get_Arcs (Model: Model_Type_Without_Finalize;
                      Source, Target: Node_Type_Without_Finalize'Class)
                      return Node_Iterator_Type is
      Handle: constant Node_Iterator_Handle :=
        librdf_model_get_arcs(Get_Handle(Model), Get_Handle(Source), Get_Handle(Target));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_model_get_targets (Model: Model_Handle; Source, Arc: Node_Handle)
                                      return Node_Iterator_Handle
     with Import, Convention=>C;

   function Get_Targets (Model: Model_Type_Without_Finalize;
                         Source, Arc: Node_Type_Without_Finalize'Class)
                         return Node_Iterator_Type is
      Handle: constant Node_Iterator_Handle :=
        librdf_model_get_targets(Get_Handle(Model), Get_Handle(Source), Get_Handle(Arc));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_model_get_source (Model: Model_Handle; Arc, Target: Node_Handle) return Node_Handle
     with Import, Convention=>C;

   function Get_Source (Model: Model_Type_Without_Finalize;
                        Arc, Target: Node_Type_Without_Finalize'Class)
                        return Node_Type is
      Handle: constant Node_Handle :=
        librdf_model_get_source(Get_Handle(Model), Get_Handle(Arc), Get_Handle(Target));
   begin
      return From_Handle(Handle);
   end;

   function librdf_model_get_arc (Model: Model_Handle; Source, Target: Node_Handle) return Node_Handle
     with Import, Convention=>C;

   function Get_Arc (Model: Model_Type_Without_Finalize;
                     Source, Target: Node_Type_Without_Finalize'Class)
                     return Node_Type is
      Handle: constant Node_Handle :=
        librdf_model_get_arc(Get_Handle(Model), Get_Handle(Source), Get_Handle(Target));
   begin
      return From_Handle(Handle);
   end;

   function librdf_model_get_target (Model: Model_Handle; Source, Arc: Node_Handle) return Node_Handle
     with Import, Convention=>C;

   function Get_Target (Model: Model_Type_Without_Finalize;
                        Source, Arc: Node_Type_Without_Finalize'Class)
                        return Node_Type is
      Handle: constant Node_Handle :=
        librdf_model_get_target(Get_Handle(Model), Get_Handle(Source), Get_Handle(Arc));
   begin
      return From_Handle(Handle);
   end;

   function librdf_model_add_submodel (Model, Submodel: Model_Handle) return int
     with Import, Convention=>C;

   procedure Add_Submodel (Model: Model_Type_Without_Finalize;
                           Submodel: Model_Type_Without_Finalize'Class) is
   begin
      if librdf_model_add_submodel(Get_Handle(Model), Get_Handle(Submodel)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_model_remove_submodel (Model, Submodel: Model_Handle) return int
     with Import, Convention=>C;

   procedure Remove_Submodel (Model: Model_Type_Without_Finalize;
                              Submodel: Model_Type_Without_Finalize'Class) is
   begin
      if librdf_model_remove_submodel(Get_Handle(Model), Get_Handle(Submodel)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_model_context_as_stream (Model: Model_Handle; Context: Node_Handle) return Stream_Handle
     with Import, Convention=>C;

   function Context_As_Stream (Model: Model_Type_Without_Finalize;
                               Context: Node_Type_Without_Finalize'Class)
                               return Stream_Type is
   begin
      return From_Non_Null_Handle(librdf_model_context_as_stream(Get_Handle(Model), Get_Handle(Context)));
   end;

   function librdf_model_contains_context (Model: Model_Handle; Context: Node_Handle) return int
     with Import, Convention=>C;

   function Contains_Context (Model: Model_Type_Without_Finalize;
                              Context: Node_Type_Without_Finalize'Class)
                              return Boolean is
   begin
      return librdf_model_contains_context(Get_Handle(Model), Get_Handle(Context)) /= 0;
   end;

   function librdf_model_supports_contexts (Model: Model_Handle) return int
     with Import, Convention=>C;

   function Supports_Context (Model: Model_Type_Without_Finalize) return Boolean is
     (librdf_model_supports_contexts(Get_Handle(Model)) /= 0);

   function librdf_model_query_execute (Model: Model_Handle; Query: Query_Handle)
                                        return Query_Results_Handle
     with Import, Convention=>C;

   function Query_Execute (Model: Model_Type_Without_Finalize;
                           Query: RDF.Redland.Query.Query_Type_Without_Finalize'Class)
                           return RDF.Redland.Query_Results.Query_Results_Type_Without_Finalize is
   begin
      return From_Non_Null_Handle(librdf_model_query_execute(Get_Handle(Model), Get_Handle(Query)));
   end;

   function librdf_model_sync (Model: Model_Handle) return int
     with Import, Convention=>C;

   procedure Sync (Model: Model_Type_Without_Finalize) is
   begin
      if librdf_model_sync(Get_Handle(Model)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_model_get_storage (Model: Model_Handle) return Storage_Handle
     with Import, Convention=>C;

   function Get_Storage (Model: Model_Type_Without_Finalize) return Storage_Type_Without_Finalize is
   begin
      return From_Handle(librdf_model_get_storage(Get_Handle(Model)));
   end;

   function librdf_model_load (Model: Model_Handle;
                               URI: URI_Handle;
                               Name, Mime_Type: chars_ptr;
                               Type_URI: URI_Handle)
                               return int
     with Import, Convention=>C;

   procedure Load (Model: Model_Type_Without_Finalize;
                   URI: URI_Type_Without_Finalize'Class;
                   Name, Mime_Type: String := "";
                   Type_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null))) is
      Name2: aliased char_array := To_C(Name);
      Mime_Type2: aliased char_array := To_C(Mime_Type);
   begin
      if librdf_model_load(Get_Handle(Model),
                           Get_Handle(URI),
                           (if Name = "" then Null_Ptr else To_Chars_Ptr(Name2'Unchecked_Access)),
                           (if Mime_Type = "" then Null_Ptr else To_Chars_Ptr(Mime_Type2'Unchecked_Access)),
                           Get_Handle(Type_URI)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   type Size_T_P is access all size_t with Convention=>C;

   function librdf_model_to_counted_string (Model: Model_Handle;
                                            URI: URI_Handle;
                                            Name, Mime_Type: chars_ptr;
                                            Type_URI: URI_Handle;
                                            Length: Size_T_P)
                                            return chars_ptr
     with Import, Convention=>C;

   function To_String (Model: Model_Type_Without_Finalize;
                       URI: URI_Type_Without_Finalize'Class;
                       Name, Mime_Type: String := "";
                       Type_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                       return String is
      Name2: aliased char_array := To_C(Name);
      Mime_Type2: aliased char_array := To_C(Mime_Type);
      Length: aliased size_t;
      Result: constant chars_ptr :=
        librdf_model_to_counted_string(Get_Handle(Model),
                                       Get_Handle(URI),
                                       (if Name = "" then Null_Ptr else To_Chars_Ptr(Name2'Unchecked_Access)),
                                       (if Mime_Type = "" then Null_Ptr else To_Chars_Ptr(Mime_Type2'Unchecked_Access)),
                                       Get_Handle(Type_URI),
                                       Length'Unchecked_Access);
   begin
      return Value_With_Possible_NULs(Convert(Result), Length);
   end;

   function librdf_model_find_statements_in_context (Model: Model_Handle;
                                                     Statement: Statement_Handle;
                                                     Context: Node_Handle)
                                                     return Stream_Handle
     with Import, Convention=>C;

   function Find_In_Context (Model: Model_Type_Without_Finalize;
                             Statement: Statement_Type_Without_Finalize'Class;
                             Context: Node_Type_Without_Finalize'Class)
                             return Stream_Type is
      Handle: constant Stream_Handle :=
        librdf_model_find_statements_in_context(Get_Handle(Model), Get_Handle(Statement), Get_Handle(Context));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_model_get_contexts (Model: Model_Handle) return Node_Iterator_Handle
     with Import, Convention=>C;

   function Get_Contexts (Model: Model_Type_Without_Finalize) return Node_Iterator_Type is
   begin
      return From_Non_Null_Handle(librdf_model_get_contexts(Get_Handle(Model)));
   end;

   function librdf_model_get_feature (Model: Model_Handle; Feature: URI_Handle) return Node_Handle
     with Import, Convention=>C;

   function Get_Feature (Model: Model_Type_Without_Finalize;
                         Feature: URI_Type_Without_Finalize'Class)
                         return Node_Type is
   begin
      return From_Handle(librdf_model_get_feature(Get_Handle(Model), Get_Handle(Feature)));
   end;

   function librdf_model_set_feature (Model: Model_Handle; Feature: URI_Handle; Value: Node_Handle)
                                      return int
     with Import, Convention=>C;

   procedure Set_Feature (Model: in out Model_Type_Without_Finalize;
                          Feature: URI_Type_Without_Finalize'Class;
                          Value: Node_Type) is
   begin
      if librdf_model_set_feature(Get_Handle(Model), Get_Handle(Feature), Get_Handle(Value)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_model_write (Model: Model_Handle; Stream: IOStream_Handle) return int
     with Import, Convention=>C;

   procedure Write (Model: Model_Type_Without_Finalize; Stream: IOStream_Type_Without_Finalize'Class) is
   begin
      if librdf_model_write(Get_Handle(Model), Get_Handle(Stream)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

end RDF.Redland.Model;
