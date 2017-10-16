with Interfaces.C; use Interfaces.C;

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

end RDF.Redland.Statement;
