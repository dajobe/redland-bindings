with RDF.Raptor.Statement;
--  with RDF.Redland.World; use RDF.Redland.World;

package RDF.Redland.Statement is

   subtype Statement_Handle is RDF.Raptor.Statement.Statement_Handle;

   type Statement_Type_Without_Finalize is new RDF.Raptor.Statement.Statement_Handled_Record.Base_Object with null record;

   function To_Raptor (Statement: Statement_Type_Without_Finalize'Class) return RDF.Raptor.Statement.Statement_Type_Without_Finalize;

   not overriding function From_Raptor (Statement: RDF.Raptor.Statement.Statement_Type_Without_Finalize'Class) return Statement_Type_Without_Finalize;

   type Statement_Part_Flags is mod 256; -- the number may change in a future version

   Subject_Part  : Statement_Part_Flags := 1;
   Predicate_Part: Statement_Part_Flags := 2;
   Object_Part   : Statement_Part_Flags := 4;
   All_Parts     : Statement_Part_Flags := Subject_Part or Predicate_Part or Object_Part;

   -- Stopped at librdf_new_statement()

end RDF.Redland.Statement;
