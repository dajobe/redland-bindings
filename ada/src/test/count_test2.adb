with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AUnit.Assertions; use AUnit.Assertions;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.URI; use RDF.Redland.URI;
with RDF.Redland.Node; use RDF.Redland.Node;
with RDF.Redland.Storage; use RDF.Redland.Storage;
with RDF.Redland.Model; use RDF.Redland.Model;
with RDF.Redland.Query; use RDF.Redland.Query;
with RDF.Redland.Query_Results; use RDF.Redland.Query_Results;
with AUnit.Test_Cases;
with Our_Test; use Our_Test;

package body Count_Test2 is

   procedure Test_Empty(T : in out Test_Cases.Test_Case'Class) is
      World: Redland_World_Type;
      Storage: Storage_Type := Create(World, "memory", "test");
      Model: Model_Type := Create(World, Storage);
      T2: Our_Test_Case renames Our_Test_Case(T);
      Directory: constant String := To_String(Get_Environment(T2).Directory);
      RDF_File: constant String := Directory & "/../data/dc.nt";
      SPARQL: constant String := "SELECT (count(*) as ?count) WHERE { ?s ?p ?o . }";
--        Query: Query_Type := Copy(Create(World, "sparql", SPARQL)); -- FIXME: Copy causes an unhandled signal
      Query: Query_Type := Create(World, "sparql", SPARQL);
      Results: Bindings_Query_Results_Type := Query_Execute(Model, Query);
      package Rows_Holder is new Ada.Containers.Indefinite_Vectors(Positive, String);
      use Ada.Containers, Rows_Holder;
      Rows: Vector;
   begin
      Load(Model, From_Filename(World, RDF_File));
      for I in Create_Bindings_Iterator(Results) loop
         declare
            L: Literal_Node_Type := Get_Binding_Value_By_Name(I, "count");
         begin
            Append(Rows, As_String(L));
         end;
      end loop;
      Assert(Length(Rows) = Count_Type(1), "count() returns one row");
      Assert(Element(Rows, 1) = "3", "count() = 3");
   end;

   function Name (T : Test_Case)
                  return Test_String is
   begin
      return Format ("Redland query");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Empty'Access, "count() = 3");
   end Register_Tests;

end Count_Test2;
