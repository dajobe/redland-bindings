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

   procedure Adjust (Object: in out Statement_Type) is
      use RDF.Raptor.Statement;
   begin
      if Get_Handle(Object) /= null then
         Set_Handle_Hack(Object, librdf_new_statement_from_statement(Get_Handle(Object)));
      end if;
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

--     procedure Finalize_Handle(Object: Statement_Type; Handle: Statement_Handle) is
--     begin
--     end;

end RDF.Redland.Statement;
