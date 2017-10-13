with RDF.Raptor.Statement;
--  with RDF.Redland.World; use RDF.Redland.World;

package RDF.Redland.Statement is

   subtype Statement_Handle is RDF.Raptor.Statement.Statement_Handle;

   type Statement_Type_Without_Finalize is new RDF.Raptor.Statement.Statement_Handled_Record.Base_Object with null record;

   function To_Raptor (Statement: Statement_Type_Without_Finalize'Class) return RDF.Raptor.Statement.Statement_Type_Without_Finalize;

   not overriding function From_Raptor (Statement: RDF.Raptor.Statement.Statement_Type_Without_Finalize'Class) return Statement_Type_Without_Finalize;

end RDF.Redland.Statement;
