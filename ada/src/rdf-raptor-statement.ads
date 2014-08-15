with Interfaces.C;
with RDF.Raptor.World;
with RDF.Raptor.World;
with RDF.Raptor.Term;

package RDF.Raptor.Statement is

   type Statement_Type is private;

   not overriding function Get_World (Statement: Statement_Type) return RDF.Raptor.World.World_Type_Without_Finalize;

   not overriding function Get_Subject   (Statement: Statement_Type) return RDF.Raptor.Term.Term_Type_Without_Finalize;
   not overriding function Get_Predicate (Statement: Statement_Type) return RDF.Raptor.Term.Term_Type_Without_Finalize;
   not overriding function Get_Object    (Statement: Statement_Type) return RDF.Raptor.Term.Term_Type_Without_Finalize;
   not overriding function Get_Graph     (Statement: Statement_Type) return RDF.Raptor.Term.Term_Type_Without_Finalize; -- may return null handle

private

   type Statement_Type is
      record
         world: access RDF.Raptor.World.World_Type_Without_Finalize;
         usage: Interfaces.C.int;
         Subject, Predicate, Object, Graph: access RDF.Raptor.Term.Term_Type_Without_Finalize;
      end record
         with Convention=>C;

end RDF.Raptor.Statement;
