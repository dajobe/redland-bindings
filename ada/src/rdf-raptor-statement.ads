-- TODO
with RDF.Auxilary.Handled_Record;
private with Interfaces.C;
with RDF.Auxilary;
with RDF.Raptor.World;

package RDF.Raptor.Statement is

--     type Raptor_Statement_Record is private;

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

--     type Raptor_Term_Type is (Raptor_Term_Type_Unknown,
--                               Raptor_Term_Type_URI,
--                               Raptor_Term_Type_Literal,
--                               Raptor_Term_Type_Blank);
--     for Raptor_Term_Type'Size use Interfaces.C.int'Size; -- hack
--     for Raptor_Term_Type use (Raptor_Term_Type_Unknown => 0,
--                               Raptor_Term_Type_URI     => 1,
--                               Raptor_Term_Type_Literal => 2,
--                               -- unused type 3
--                               Raptor_Term_Type_Blank   => 4);
--
--     type Term_Kind_Type is (URI, Literal, Blank);
--
--     type Raptor_Term_Value (Term_Kind: Term_Kind_Type) is
--        record
--           case Term_Kind is
--              when URI     => URI: Raptor_URI;
--              when Literal => Literal: Raptor_Term_Literal_Value;
--              when Blank   => Blank: Raptor_Term_Blank_Value;
--           end case;
--        end record
--           with Unchecked_Union, Convention => C;
--
--     type Raptor_Term is
--        record
--           World: access RDF.Raptor.World.World_Type;
--           Usage: Interfaces.C.int;
--           The_Type: Raptor_Term_Type;
--           Value: Raptor_Term_Value;
--        end record
--           with Convention=>C;
--
--     type Raptor_Statement_Record is
--        record
--           world: access RDF.Raptor.World.World_Type;
--           usage: Interfaces.C.int;
--           Subject, Predicate, Object, Graph: access Raptor_Term;
--        end record
--           with Convention=>C;

end RDF.Raptor.Statement;
