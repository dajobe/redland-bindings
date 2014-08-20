with AUnit.Assertions; use AUnit.Assertions;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Namespaces; use RDF.Raptor.Namespaces;
with RDF.Raptor.Namespaces_Stacks; use RDF.Raptor.Namespaces_Stacks;
with RDF.Raptor.World;

package body Namespaces_Test is

   procedure Test_Stacks(T : in out Test_Cases.Test_Case'Class) is
      World: RDF.Raptor.World.World_Type;

      Stack: Namespace_Stack_Type := Create_Stack (World, XML_Type);

      URI_1_Obj: constant URI_Type := From_String(World, "http://www.w3.org/1999/xhtml/");

      NS1: Namespace_Type := New_Namespace (Stack, "xhtml", "http://www.w3.org/1999/xhtml/", 1);
      NS2: Namespace_Type := From_URI (Stack, "xhtml", URI_1_Obj, 1);
   begin
      Assert (To_String(Get_URI(NS1)) = "http://www.w3.org/1999/xhtml/", "Check namespace URI");
      Assert (To_String(Get_URI(NS2)) = "http://www.w3.org/1999/xhtml/", "Check namespace URI");
      Assert (Get_Prefix(NS1) = "xhtml", "Check namespace prefix");
      Assert (Get_Prefix(NS2) = "xhtml", "Check namespace prefix");
   end;

   function Name (T : Test_Case)
                  return Test_String is
   begin
      return Format ("Namespaces");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Stacks'Access, "Testing namespace stacks");
   end Register_Tests;

end Namespaces_Test;
