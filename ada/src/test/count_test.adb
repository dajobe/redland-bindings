with Ada.Strings.Unbounded;
with AUnit.Test_Cases;
with AUnit.Assertions;
with RDF.Auxiliary;
with RDF.Raptor.World;
with RDF.Raptor.IOStream;
with RDF.Raptor.URI;
with RDF.Rasqal.World;
with RDF.Rasqal.Data_Graph; use RDF.Rasqal.Data_Graph;
with RDF.Rasqal.Query; use RDF.Rasqal.Query;
with RDF.Rasqal.Query_Results; use RDF.Rasqal.Query_Results;

package body Count_Test is

   procedure Test_Count (T : in out Test_Cases.Test_Case'Class) is
      use Ada.Strings.Unbounded;
      use RDF.Raptor.URI;
      use RDF.Raptor.IOStream;
      use RDF.Rasqal.World;
      T2: Test_Case renames Test_Case(T);
      Directory: constant String := To_String(Get_Environment(T2).Directory);
      RDF_File: constant String := Directory & "/../data/dc.nt";
      SPARQL: constant String := "SELECT (count(*) as ?count) WHERE { ?s ?p ?o . }";

      World: RDF.Rasqal.World.World_Type := RDF.Rasqal.World.Open;
      Graph_Stream: RDF.Raptor.IOStream.Stream_Type := From_Filename(Get_Raptor(World), RDF_File);
      Graph: Data_Graph_Type := From_IOStream (World,
                                               Graph_Stream,
                                               Base_URI => RDF.Raptor.URI.From_String(Get_Raptor(World), "http://example.org"),
                                               Name_URI => URI_Type'(From_Handle(null)),
                                               Flags => RDF.Rasqal.Data_Graph.Background,
                                               Format_Type => RDF.Auxiliary.String_Holders.Empty_Holder,
                                               Format_Name => RDF.Auxiliary.String_Holders.Empty_Holder,
                                               Format_URI => URI_Type'(From_Handle(null)));
      Query: RDF.Rasqal.Query.Query_Type;
   begin
      Prepare(Query, SPARQL);
      Add_Data_Graph(Query, Graph);
      declare
         Results: Query_Results_Type := Execute(Query);
      begin
         null; -- FIXME
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
