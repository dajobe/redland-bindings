package body RDF.Raptor.Statement is

   use RDF.Raptor.World;

   function Get_World (Statement: Statement_Type) return RDF.Raptor.World.World_Type_Without_Finalize is
   begin
--        return Statement.World.all; -- does not work, so the below hack:
      return S: RDF.Raptor.World.World_Type_Without_Finalize do
         Set_Handle_Hack(S, Get_Handle(Statement.World.all));
      end return;
   end;

   function Get_Subject   (Statement: Statement_Type) return RDF.Raptor.Term.Term_Type_Without_Finalize is (Statement.Subject.all);
   function Get_Predicate (Statement: Statement_Type) return RDF.Raptor.Term.Term_Type_Without_Finalize is (Statement.Predicate.all);
   function Get_Object    (Statement: Statement_Type) return RDF.Raptor.Term.Term_Type_Without_Finalize is (Statement.Object.all);
   function Get_Graph     (Statement: Statement_Type) return RDF.Raptor.Term.Term_Type_Without_Finalize is (Statement.Graph.all); -- may return null handle

end RDF.Raptor.Statement;
