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

   function Get_Subject (Statement: Statement_Type_Without_Finalize) return Node_Type_Without_Finalize is
   begin
      return From_Handle(librdf_statement_get_subject(Get_Handle(Statement)));
   end;

end RDF.Redland.Statement;
