with RDF.Auxiliary; use RDF.Auxiliary;
with RDF.Raptor.IOStream; use RDF.Raptor.IOStream;
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

   not overriding function Get_Subject (Statement: Statement_Type_Without_Finalize)
                                        return Node_Type_Without_Finalize;
   not overriding function Get_Predicate (Statement: Statement_Type_Without_Finalize)
                                        return Node_Type_Without_Finalize;
   not overriding function Get_Object (Statement: Statement_Type_Without_Finalize)
                                        return Node_Type_Without_Finalize;

   not overriding procedure Set_Subject (Statement: Statement_Type_Without_Finalize;
                                         Node: Node_Type_Without_Finalize'Class);
   not overriding procedure Set_Predicate (Statement: Statement_Type_Without_Finalize;
                                           Node: Node_Type_Without_Finalize'Class);
   not overriding procedure Set_Object (Statement: Statement_Type_Without_Finalize;
                                        Node: Node_Type_Without_Finalize'Class);

   not overriding function Is_Complete (Statement: Statement_Type_Without_Finalize) return Boolean;

   not overriding procedure Print (Statement: Statement_Type_Without_Finalize; File: C_File_Access);

   not overriding function Equals (Left, Right: Statement_Type_Without_Finalize) return Boolean;

   overriding function "=" (Left, Right: Statement_Type_Without_Finalize) return Boolean
     renames Equals;

   not overriding function Match (Statement, Partial: Statement_Type_Without_Finalize) return Boolean;

   not overriding function Encode (World: Redland_World_Type_Without_Finalize'Class;
                                   Statement: Statement_Type_Without_Finalize)
                                   return String;

   not overriding function Encode_Parts (World: Redland_World_Type_Without_Finalize'Class;
                                         Statement: Statement_Type_Without_Finalize;
                                         Context_Node: Node_Type_Without_Finalize'Class;
                                         Fields: Statement_Part_Flags)
                                         return String;

   -- librdf_statement_decode2() not implemented (not so important and somehow hard to do)

   not overriding procedure Write (Statement: Statement_Type_Without_Finalize; Stream: Base_IOStream_Type'Class);

   package Finalizer is new With_Finalization(Statement_Type_Without_Finalize);

   type Statement_Type is new Finalizer.Derived with null record;

   overriding function Adjust_Handle (Object: Statement_Type; Handle: Statement_Handle) return Statement_Handle;

   -- librdf_new_statement_from_statement2() not bound.
   -- (It is unclear how this would interact with Ada copying.)

   overriding procedure Finalize_Handle(Object: Statement_Type; Handle: Statement_Handle);

   not overriding function Create (World: Redland_World_Type_Without_Finalize'Class) return Statement_Type;

   not overriding function From_Nodes (World: Redland_World_Type_Without_Finalize'Class;
                                       Subject, Predicate, Object: Node_Type_Without_Finalize'Class)
                                       return Statement_Type;

   -- librdf_statement_init() not bound because we don't support statistially declared objects.

end RDF.Redland.Statement;
