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

end RDF.Redland.Statement;
