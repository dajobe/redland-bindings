with Interfaces.C.Strings; use Interfaces.C.Strings;

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
                    Options: String)
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

   procedure Finalize_Handle (Object: Model_Type; Handle: Model_Handle) is
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

   procedure Add (Model: Model_Type_Without_Finalize; Subject, Predicate, Object: Node_Type_Without_Finalize'Class) is
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

   function librdf_model_add_statement (Model: Model_Handle; Statement: Statement_Handle) return int
     with Import, Convention=>C;

   procedure Add (Model: Model_Type_Without_Finalize; Statement: Statement_Type_Without_Finalize'Class) is
   begin
      if librdf_model_add_statement(Get_Handle(Model), Get_Handle(Statement)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_model_add_statements (Model_Type: Model_Handle; Statements: Stream_Handle) return int
     with Import, Convention=>C;

   procedure Add (Model: Model_Type_Without_Finalize; Statements: Stream_Type_Without_Finalize'Class) is
   begin
      if librdf_model_add_statements(Get_Handle(Model), Get_Handle(Statements)) /= 0 then
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

end RDF.Redland.Model;
