with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;
with AUnit.Assertions; use AUnit.Assertions;
with RDF.Auxiliary;
with RDF.Raptor.World;
with RDF.Raptor.IOStream;
with RDF.Raptor.URI;
with RDF.Rasqal.World;
with RDF.Rasqal.Literal;
with RDF.Rasqal.Data_Graph; use RDF.Rasqal.Data_Graph;
with RDF.Rasqal.Query; use RDF.Rasqal.Query;
with RDF.Rasqal.Query_Results; use RDF.Rasqal.Query_Results;

package body Count_Test is

   procedure Test_Count (T : in out Test_Cases.Test_Case'Class) is
      use Ada.Strings.Unbounded;
      use RDF.Auxiliary.String_Holders;
      use RDF.Raptor.URI;
      use RDF.Raptor.IOStream;
      use RDF.Rasqal.World;
      T2: Test_Case renames Test_Case(T);
      Directory: constant String := To_String(Get_Environment(T2).Directory);
      RDF_File: constant String := Directory & "/../data/dc.nt";
      SPARQL: constant String := "SELECT (count(*) as ?count) WHERE { ?s ?p ?o . }";

      World: Rasqal_World_Type := Open;
      Graph_Stream: RDF.Raptor.IOStream.IOStream_Type := From_Filename(Get_Raptor(World), RDF_File);
      Graph: Data_Graph_Type := From_IOStream(World,
                                              Graph_Stream,
                                              Base_URI => From_String(Get_Raptor(World), "http://example.org")
                                              --                                                 Name_URI => URI_Type'(From_Handle(null)),
                                              --                                                 Flags => Background,
                                              --                                                 Format_Type => Empty_Holder,
                                              --                                                 Format_Name => Empty_Holder,
                                              --                                                 Format_URI => URI_Type'(From_Handle(null))
                                             );
      Query: RDF.Rasqal.Query.Query_Type := New_Query(World, Empty_Holder, Empty_Holder);
   begin
      Prepare(Query, SPARQL);
      Add_Data_Graph(Query, Graph);
      declare
         Results: Bindings_Query_Results_Type := Execute(Query);
         package Rows_Holder is new Ada.Containers.Indefinite_Vectors(Positive, String);
         use Ada.Containers, Rows_Holder;
         Rows: Vector;
      begin
         Assert(Is_Bindings(Results), "Result is bindings");
         for I in Create_Bindings_Iterator(Results) loop
            declare
               use RDF.Rasqal.Literal;
               L: Literal_Type_Without_Finalize := Get_Binding_Value_By_Name(I, "count");
            begin
               Append(Rows, As_String(L));
            end;
         end loop;
         Assert(Length(Rows) = Count_Type(1), "count() returns one row");
         Assert(Element(Rows, 1) = "3", "count() = 3");
      end;
   end;

   function Name (T : Test_Case)
                  return Test_String is
   begin
      return Format ("Test SPARQL count()");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Count'Access, "count() = 3");
   end Register_Tests;

end Count_Test;
