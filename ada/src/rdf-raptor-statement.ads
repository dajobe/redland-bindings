with Interfaces.C;
with RDF.Auxilary.Handled_Record;
with RDF.Raptor.World;
with RDF.Raptor.Term; use RDF.Raptor.Term;

package RDF.Raptor.Statement is

   type Statement_Record is private;

   package Statement_Handled_Record is new RDF.Auxilary.Handled_Record(Statement_Record);

   type Statement_Type_Without_Finalize is new Statement_Handled_Record.Base_Object with null record;

   subtype Statement_Handle is Statement_Handled_Record.Access_Type;

   not overriding function Get_World (Statement: Statement_Type_Without_Finalize) return RDF.Raptor.World.World_Type_Without_Finalize;

   not overriding function Get_Subject   (Statement: Statement_Type_Without_Finalize) return RDF.Raptor.Term.Term_Type_Without_Finalize;
   not overriding function Get_Predicate (Statement: Statement_Type_Without_Finalize) return RDF.Raptor.Term.Term_Type_Without_Finalize;
   not overriding function Get_Object    (Statement: Statement_Type_Without_Finalize) return RDF.Raptor.Term.Term_Type_Without_Finalize;
   not overriding function Get_Graph     (Statement: Statement_Type_Without_Finalize) return RDF.Raptor.Term.Term_Type_Without_Finalize; -- may return null handle

   not overriding function Compare (Left, Right: Statement_Type_Without_Finalize) return RDF.Auxilary.Comparison_Result;

   not overriding function Equals (Left, Right: Statement_Type_Without_Finalize) return Boolean;

   overriding function "=" (Left, Right: Statement_Type_Without_Finalize) return Boolean renames Equals;

   not overriding procedure Print (Statement: Statement_Type_Without_Finalize; File: RDF.Auxilary.C_File_Access);

   not overriding procedure Print_As_Ntriples (Statement: Statement_Type_Without_Finalize; File: RDF.Auxilary.C_File_Access);

   -- raptor_statement_init(), raptor_statement_clear() are not boound, because they are probably internal

   -- TODO: Stopped at raptor_statement_ntriples_write()

   not overriding function Copy (Object: Statement_Type_Without_Finalize'Class) return Statement_Type_Without_Finalize;

   type Statement_Type is new Statement_Type_Without_Finalize with null record;

   overriding procedure Adjust (Object: in out Statement_Type);

   overriding procedure Finalize_Handle (Object: Statement_Type; Handle: Statement_Handle);

   -- Returns False for certain types which automatically finalize handles and so are not appropriate for objects owned by a statement
--     function No_Auto_Finalization (Term: RDF.Raptor.Term.Term_Type_Without_Finalize'Class) return Boolean;

--     not overriding function New_Statement (World: RDF.Raptor.World.World_Type_Without_Finalize) return Statement_Type;

   -- Makes copies of the terms (unlike the C library)
   not overriding function New_Statement (World: RDF.Raptor.World.World_Type_Without_Finalize'Class;
                                          Subject, Predicate, Object: RDF.Raptor.Term.Term_Type_Without_Finalize'Class;
                                          Graph: RDF.Raptor.Term.Term_Type_Without_Finalize'Class := RDF.Raptor.Term.Term_Type_Without_Finalize'(From_Handle(null)))
                                          return Statement_Type;

   -- Does not make copies of the terms (as the C library)
   not overriding function New_Statement_Without_Copies (World: RDF.Raptor.World.World_Type_Without_Finalize'Class;
                                                         Subject, Predicate, Object: RDF.Raptor.Term.Term_Type_Without_Finalize;
                                                         Graph: RDF.Raptor.Term.Term_Type_Without_Finalize := From_Handle(null))
                                                         return Statement_Type;
--     not overriding function New_Statement (World: RDF.Raptor.World.World_Type_Without_Finalize'Class;
--                                            Subject, Predicate, Object, Graph: RDF.Raptor.Term.Term_Type_Without_Finalize'Class)
--                                            return Statement_Type
--        with Pre => No_Auto_Finalization(Subject) and No_Auto_Finalization(Predicate) and No_Auto_Finalization(Object) and No_Auto_Finalization(Graph);

private

   type Statement_Record is
      record
         world: RDF.Raptor.World.Handle_Type;
         usage: Interfaces.C.int;
         Subject, Predicate, Object, Graph: RDF.Raptor.Term.Term_Handle;
      end record
         with Convention=>C;

end RDF.Raptor.Statement;
