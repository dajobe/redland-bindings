-- TODO
with RDF.Auxilary.Handled_Record;
private with Interfaces.C;
with RDF.Auxilary;
with RDF.Raptor.World;

package RDF.Raptor.Statement is

   type Raptor_Statement_Record is private;

--     type Raptor_Statement_Record_Access is access Raptor_Statement_Record
--        with Convention=>C;
--
--     package My_Handled_Record is
--       new RDF.Auxilary.Handled_Record(Raptor_Statement_Record, Raptor_Statement_Record_Access);
--
--     subtype Raptor_Statement is My_Handled_Record.Base_Object;
--
--     type Raptor_Statement_Handler is abstract tagged null record;
--
--     procedure Handle (Handler: Raptor_Statement_Handler) is abstract;
--
--     procedure C_Handler (User_Data: RDF.Auxilary.Dummy_Record_Access; Statement: Raptor_Statement_Record_Access);

private

   type Raptor_Statement_Record is
      record
         world: access RDF.Raptor.World.World_Type;
         usage: Interfaces.C.int;
         -- FIXME: add these
--  raptor_term* subject;
--  raptor_term* predicate;
--  raptor_term* object;
--  raptor_term* graph;
      end record
         with Convention=>C;

end RDF.Raptor.Statement;
