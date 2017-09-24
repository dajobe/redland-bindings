with AUnit.Assertions; use AUnit.Assertions;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.World;
with Ada.Text_IO;

package body URI_Test is

   procedure Test_URIs(T : in out Test_Cases.Test_Case'Class) is
      World: RDF.Raptor.World.Raptor_World_Type;
      URI_1: constant URI_String := "http://example.org/xyz";
      URI_3: constant URI_String := "http://example.org/123";
      URI_1_Obj: constant URI_Type := From_String(World, URI_1);
      URI_2_Obj: constant URI_Type := From_String(World, URI_1);
      URI_3_Obj: constant URI_Type := From_String(World, URI_3);
      URI_4_Obj: constant URI_Type := From_String(World, "http://example.org");
   begin
      Assert (To_String(URI_1_Obj) = URI_1, "Converting URI to string");
      Assert (URI_1_Obj = URI_2_Obj, "Comparing identical URIs");
      Assert (URI_1_Obj /= URI_3_Obj, "Comparing different URIs");
      Assert (To_String(From_URI_Relative_To_Base(World, URI_1_Obj, "zxc")) = "http://example.org/zxc", "Relative URI to base");
      Assert (To_String(For_Retrieval(URI_4_Obj)) = "http://example.org/", "URI for retrieval");
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
