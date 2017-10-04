with Interfaces.C;
with RDF.Auxiliary.Handled_Record;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.Term; use RDF.Raptor.Term;
with RDF.Raptor.IOStream; use RDF.Raptor.IOStream;

package RDF.Raptor.Statement is

   type Statement_Record is private;

   type Statement_Record_Access is access Statement_Record with Convention=>C;

   package Statement_Handled_Record is new RDF.Auxiliary.Handled_Record(Statement_Record, Statement_Record_Access);

   type Statement_Type_Without_Finalize is new Statement_Handled_Record.Base_Object with null record;

   subtype Statement_Handle is Statement_Handled_Record.Access_Type;

   not overriding function Get_World (Statement: Statement_Type_Without_Finalize) return Raptor_World_Type_Without_Finalize;

   not overriding function Get_Subject   (Statement: Statement_Type_Without_Finalize) return Term_Type_Without_Finalize;
   not overriding function Get_Predicate (Statement: Statement_Type_Without_Finalize) return Term_Type_Without_Finalize;
   not overriding function Get_Object    (Statement: Statement_Type_Without_Finalize) return Term_Type_Without_Finalize;
   not overriding function Get_Graph     (Statement: Statement_Type_Without_Finalize) return Term_Type_Without_Finalize; -- may return null handle

   not overriding function Compare (Left, Right: Statement_Type_Without_Finalize) return RDF.Auxiliary.Comparison_Result;

   not overriding function Equals (Left, Right: Statement_Type_Without_Finalize) return Boolean;

   overriding function "=" (Left, Right: Statement_Type_Without_Finalize) return Boolean renames Equals;

   not overriding procedure Print (Statement: Statement_Type_Without_Finalize; File: RDF.Auxiliary.C_File_Access);

   not overriding procedure Print_As_Ntriples (Statement: Statement_Type_Without_Finalize; File: RDF.Auxiliary.C_File_Access);

   -- raptor_statement_init(), raptor_statement_clear() are not boound, because they are probably internal

   not overriding procedure Ntriples_Write (Statement: Statement_Type_Without_Finalize;
                                            Stream: Base_Stream_Type'Class;
                                            Write_Graph_Term: Boolean);

   not overriding function Copy (Object: Statement_Type_Without_Finalize'Class) return Statement_Type_Without_Finalize;

   type Statement_Type is new Statement_Type_Without_Finalize with null record;

   overriding procedure Adjust (Object: in out Statement_Type);

   overriding procedure Finalize_Handle (Object: Statement_Type; Handle: Statement_Handle);

   -- Returns False for certain types which automatically finalize handles and so are not appropriate for objects owned by a statement
   --     function No_Auto_Finalization (Term: Term_Type_Without_Finalize'Class) return Boolean;

   not overriding function New_Statement (World: Raptor_World_Type_Without_Finalize'Class) return Statement_Type;

   -- Makes copies of the terms (unlike the C library)
   not overriding function New_Statement (World: Raptor_World_Type_Without_Finalize'Class;
                                          Subject, Predicate, Object: Term_Type_Without_Finalize'Class;
                                          Graph: Term_Type_Without_Finalize'Class := Term_Type_Without_Finalize'(From_Handle(null)))
                                          return Statement_Type;

   -- Does not make copies of the terms (as the C library)
   not overriding function New_Statement_Without_Copies (World: Raptor_World_Type_Without_Finalize'Class;
                                                         Subject, Predicate, Object: Term_Type_Without_Finalize'Class;
                                                         Graph: Term_Type_Without_Finalize'Class := Term_Type_Without_Finalize'(From_Handle(null)))
                                                         return Statement_Type;
   --     not overriding function New_Statement (World: Raptor_World_Type_Without_Finalize'Class;
   --                                            Subject, Predicate, Object, Graph: Term_Type_Without_Finalize'Class)
   --                                            return Statement_Type
   --        with Pre => No_Auto_Finalization(Subject) and No_Auto_Finalization(Predicate) and No_Auto_Finalization(Object) and No_Auto_Finalization(Graph);

private

   type Statement_Record is
      record
         world: Raptor_World_Handle;
         usage: Interfaces.C.int;
         Subject, Predicate, Object, Graph: Term_Handle;
      end record
     with Convention=>C;

end RDF.Raptor.Statement;
