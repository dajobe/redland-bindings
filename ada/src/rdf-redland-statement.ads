with RDF.Raptor.Statement;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.Node; use RDF.Redland.Node;

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

   not overriding procedure Clear (Statement: in out Statement_Type_Without_Finalize);

   type Statement_Type is new Statement_Type_Without_Finalize with null record;

   overriding procedure Adjust(Object: in out Statement_Type);

   -- librdf_new_statement_from_statement2() not bound.
   -- (It is unclear how this would interact with Ada copying.)

--     overriding procedure Finalize_Handle(Object: Statement_Type; Handle: Statement_Handle);

   -- Stopped at librdf_free_statement()

   not overriding function Create (World: Redland_World_Type_Without_Finalize'Class) return Statement_Type;

   not overriding function From_Nodes (World: Redland_World_Type_Without_Finalize'Class;
                                       Subject, Predicate, Object: Node_Type_Without_Finalize'Class)
                                       return Statement_Type;

   -- librdf_statement_init() not bound because we don't support statistially declared objects.

end RDF.Redland.Statement;
