with AUnit.Assertions; use AUnit.Assertions;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Term; use RDF.Raptor.Term;
with RDF.Raptor.Statement; use RDF.Raptor.Statement;
with RDF.Raptor.World; use RDF.Raptor.World;
with Ada.Text_IO;
with RDF.Auxiliary;

package body Statement_Test is

   procedure Test_Statements(T : in out Test_Cases.Test_Case'Class) is
      World: Raptor_World_Type;

      URI_1: constant URI_String := "http://example.org/xyz";
      URI_2: constant URI_String := "http://example.org/qqq";
      URI_3: constant URI_String := "http://example.org/123";

      Term_1: Term_Type := From_URI_String(World, URI_1);
      Term_2: Term_Type := From_URI_String(World, URI_2);
      Term_3: Term_Type := From_URI_String(World, URI_3);

      St: Statement_Type := Create(World, Term_1, Term_2, Term_3);
   begin
      Assert(To_String(Get_Subject  (St)) = String("<" & URI_1 & ">"), "Subject matches");
      Assert(To_String(Get_Predicate(St)) = String("<" & URI_2 & ">"), "Predicate matches");
      Assert(To_String(Get_Object   (St)) = String("<" & URI_3 & ">"), "Object matches");
   end;

   function Name (T : Test_Case)
                  return Test_String is
   begin
      return Format ("RDF statements");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Statements'Access, "Testing statements");
   end Register_Tests;

end Statement_Test;
