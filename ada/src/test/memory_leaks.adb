with Ada.Containers;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with RDF.Auxiliary; use RDF.Auxiliary;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.IOStream; use RDF.Raptor.IOStream;
with RDF.Rasqal.World; use RDF.Rasqal.World;
with RDF.Rasqal.Data_Graph; use RDF.Rasqal.Data_Graph;
with RDF.Rasqal.Query; use RDF.Rasqal.Query;
with RDF.Rasqal.Query_Results; use RDF.Rasqal.Query_Results;
with RDF.Rasqal.Literal; use RDF.Rasqal.Literal;

procedure Memory_Leaks is
--     T2: Test_Case renames Test_Case(T);
   Directory: constant String := ".";
   RDF_File: constant String := Directory & "/../data/dc.nt";
   SPARQL: constant String := "SELECT (count(*) as ?count) WHERE { ?s ?p ?o . }";

   World: Rasqal_World_Type := Open;
   Graph_Stream: RDF.Raptor.IOStream.IOStream_Type := From_Filename(Get_Raptor(World), RDF_File);
   Graph: Data_Graph_Type := From_IOStream(World,
                                           Graph_Stream,
                                           Base_URI => From_String(Get_Raptor(World), "http://example.org"));
   use RDF.Auxiliary.String_Holders;
   Query: RDF.Rasqal.Query.Query_Type := Create(World, Empty_Holder, Empty_Holder);
begin
   Prepare(Query, SPARQL);
   Add_Data_Graph(Query, Graph);
   declare
      Results: Bindings_Query_Results_Type := Execute(Query);
      package Rows_Holder is new Ada.Containers.Indefinite_Vectors(Positive, String);
      use Ada.Containers, Rows_Holder;
      Rows: Vector;
   begin
--        Assert(Is_Bindings(Results), "Result is bindings");
      for I in Create_Bindings_Iterator(Results) loop
         declare
            L: Literal_Type_Without_Finalize := Get_Binding_Value_By_Name(I, "count");
         begin
            Append(Rows, As_String(L));
         end;
      end loop;
--        Assert(Length(Rows) = Count_Type(1), "count() returns one row");
--        Assert(Element(Rows, 1) = "3", "count() = 3");
   end;
end;
