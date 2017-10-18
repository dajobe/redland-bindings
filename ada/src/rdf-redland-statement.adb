with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body RDF.Redland.Statement is

   function To_Raptor (Statement: Statement_Type_Without_Finalize'Class)
                       return RDF.Raptor.Statement.Statement_Type_Without_Finalize is
      use RDF.Raptor.Statement;
   begin
      return From_Handle(Get_Handle(Statement));
   end;

   function From_Raptor (Statement: RDF.Raptor.Statement.Statement_Type_Without_Finalize'Class)
                         return Statement_Type_Without_Finalize is
      use RDF.Raptor.Statement;
   begin
      return From_Handle(Get_Handle(Statement));
   end;

   function librdf_new_statement (World: Redland_World_Handle) return Statement_Handle
     with Import, Convention=>C;

   function Create (World: Redland_World_Type_Without_Finalize'Class) return Statement_Type is
   begin
      return From_Non_Null_Handle(librdf_new_statement(Get_Handle(World)));
   end;

   function librdf_new_statement_from_statement (Statement: Statement_Handle) return Statement_Handle
     with Import, Convention=>C;

   function Adjust_Handle (Object: Statement_Type; Handle: Statement_Handle)
                           return Statement_Handle is
   begin
      return librdf_new_statement_from_statement(Handle);
   end;

   function librdf_new_statement_from_nodes (World: Redland_World_Handle;
                                             Subject, Predicate, Object: Node_Handle)
                                             return Statement_Handle
     with Import, Convention=>C;

   function From_Nodes (World: Redland_World_Type_Without_Finalize'Class;
                        Subject, Predicate, Object: Node_Type_Without_Finalize'Class)
                        return Statement_Type is
      Handle: constant Statement_Handle := librdf_new_statement_from_nodes(Get_Handle(World),
                                                                           Get_Handle(Subject),
                                                                           Get_Handle(Predicate),
                                                                           Get_Handle(Object));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   procedure librdf_statement_clear (Statement: Statement_Handle)
     with Import, Convention=>C;

   procedure Clear (Statement: in out Statement_Type_Without_Finalize) is
   begin
      librdf_statement_clear(Get_Handle(Statement));
   end;

   procedure librdf_free_statement (Statement: Statement_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Statement_Type; Handle: Statement_Handle) is
   begin
      librdf_free_statement(Handle);
   end;

   function librdf_statement_get_subject (Statement: Statement_Handle) return Node_Handle
     with Import, Convention=>C;
   function librdf_statement_get_predicate (Statement: Statement_Handle) return Node_Handle
     with Import, Convention=>C;
   function librdf_statement_get_object (Statement: Statement_Handle) return Node_Handle
     with Import, Convention=>C;

   function Get_Subject (Statement: Statement_Type_Without_Finalize) return Node_Type_Without_Finalize is
     (From_Handle(librdf_statement_get_subject(Get_Handle(Statement))));
   function Get_Predicate (Statement: Statement_Type_Without_Finalize) return Node_Type_Without_Finalize is
     (From_Handle(librdf_statement_get_predicate(Get_Handle(Statement))));
   function Get_Object (Statement: Statement_Type_Without_Finalize) return Node_Type_Without_Finalize is
     (From_Handle(librdf_statement_get_object(Get_Handle(Statement))));

   function librdf_new_node_from_node (Node: Node_Handle) return Node_Handle
     with Import, Convention=>C;

   procedure librdf_statement_set_subject (Statement: Statement_Handle; Node: Node_Handle)
     with Import, Convention=>C;
   procedure librdf_statement_set_predicate (Statement: Statement_Handle; Node: Node_Handle)
     with Import, Convention=>C;
   procedure librdf_statement_set_object (Statement: Statement_Handle; Node: Node_Handle)
     with Import, Convention=>C;

   procedure Set_Subject (Statement: Statement_Type_Without_Finalize;
                          Node: Node_Type_Without_Finalize'Class) is
   begin
      librdf_statement_set_subject(Get_Handle(Statement),
                                   librdf_new_node_from_node(Get_Handle(Node)));
   end;

   procedure Set_Predicate (Statement: Statement_Type_Without_Finalize;
                            Node: Node_Type_Without_Finalize'Class) is
   begin
      librdf_statement_set_predicate(Get_Handle(Statement),
                                     librdf_new_node_from_node(Get_Handle(Node)));
   end;

   procedure Set_Object (Statement: Statement_Type_Without_Finalize;
                         Node: Node_Type_Without_Finalize'Class) is
   begin
      librdf_statement_set_object(Get_Handle(Statement),
                                  librdf_new_node_from_node(Get_Handle(Node)));
   end;

   function librdf_statement_is_complete (Statement: Statement_Handle) return int
     with Import, Convention=>C;

   function Is_Complete (Statement: Statement_Type_Without_Finalize) return Boolean is
      (librdf_statement_is_complete(Get_Handle(Statement)) /= 0);

   procedure librdf_statement_print (Statement: Statement_Handle; File: C_File_Access)
     with Import, Convention=>C;

   procedure Print (Statement: Statement_Type_Without_Finalize; File: C_File_Access) is
   begin
      librdf_statement_print(Get_Handle(Statement), File);
   end;

   function librdf_statement_equals (Left, Right: Statement_Handle) return int
     with Import, Convention=>C;

   function Equals (Left, Right: Statement_Type_Without_Finalize) return Boolean is
     (librdf_statement_equals(Get_Handle(Left), Get_Handle(Right)) /= 0);

   function librdf_statement_match (Statement, Partial: Statement_Handle) return int
     with Import, Convention=>C;

   function Match (Statement, Partial: Statement_Type_Without_Finalize) return Boolean is
     (librdf_statement_match(Get_Handle(Statement), Get_Handle(Partial)) /= 0);

   function librdf_statement_encode2 (World: Redland_World_Handle;
                                      Statement: Statement_Handle;
                                      Buffer: chars_ptr;
                                      Length: size_t)
                                      return size_t
     with Import, Convention=>C;

   function Encode (World: Redland_World_Type_Without_Finalize'Class;
                    Statement: Statement_Type_Without_Finalize)
                    return String is
      Length: size_t :=
        librdf_statement_encode2(Get_Handle(World), Get_Handle(Statement), Null_Ptr, 0);
--        Buffer: aliased char_array(1..Length); -- GCC 7.2.0 warning
      Buffer: aliased char_array := (1..Length => nul);
   begin
      Length := librdf_statement_encode2(Get_Handle(World),
                                         Get_Handle(Statement),
                                         To_Chars_Ptr(Buffer'Unchecked_Access),
                                         Length);
      return To_Ada(Buffer, Trim_Nul=>False);
   end;

   function librdf_statement_encode_parts2 (World: Redland_World_Handle;
                                            Statement: Statement_Handle;
                                            Context_Node: Node_Handle;
                                            Buffer: chars_ptr;
                                            Length: size_t;
                                            Fields: Statement_Part_Flags)
                                            return size_t
     with Import, Convention=>C;

   function Encode_Parts (World: Redland_World_Type_Without_Finalize'Class;
                          Statement: Statement_Type_Without_Finalize;
                          Context_Node: Node_Type_Without_Finalize'Class;
                          Fields: Statement_Part_Flags)
                          return String is
      Length: size_t := librdf_statement_encode_parts2(Get_Handle(World),
                                                       Get_Handle(Statement),
                                                       Get_Handle(Context_Node),
                                                       Null_Ptr,
                                                       0,
                                                       Fields);
--        Buffer: aliased char_array(1..Length); -- GCC 7.2.0 warning
      Buffer: aliased char_array := (1..Length => nul);
   begin
      Length := librdf_statement_encode_parts2(Get_Handle(World),
                                               Get_Handle(Statement),
                                               Get_Handle(Context_Node),
                                               To_Chars_Ptr(Buffer'Unchecked_Access),
                                               Length,
                                               Fields);
      return To_Ada(Buffer, Trim_Nul=>False);
   end;

   function librdf_statement_write (Statement: Statement_Handle; Stream: IOStream_Handle) return int
     with Import, Convention=>C;

   procedure Write (Statement: Statement_Type_Without_Finalize; Stream: Base_IOStream_Type'Class) is
   begin
      if librdf_statement_write(Get_Handle(Statement), Get_Handle(Stream)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

end RDF.Redland.Statement;
