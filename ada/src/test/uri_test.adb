--  with Interfaces.C; use Interfaces.C;
--  with Interfaces.C.Strings; use Interfaces.C.Strings;
--  with AUnit.Test_Cases;
with AUnit.Assertions; use AUnit.Assertions;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.World;
--  with RDF.Raptor.Iostream; use RDF.Raptor.Iostream;
--  with Ada.Text_IO;

package body URI_Test is

   procedure Test_URIs(T : in out Test_Cases.Test_Case'Class) is
      World: RDF.Raptor.World.World_Type;
      URI_1: constant String := "http://example.org/xyz";
      URI_1_Obj: constant URI_Type := From_String(World, URI_1);
   begin
      Assert (To_String(URI_1_Obj) = URI_1, "Converting URI to string");
   end;

   function Name (T : Test_Case)
                  return Test_String is
   begin
      return Format ("URIs");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_URIs'Access, "Testing URIs");
   end Register_Tests;

end URI_Test;
