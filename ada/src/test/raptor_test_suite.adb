with Basic_Test;
with Constants_Test;
with Iostreams_Test;
with URI_Test;
with Namespaces_Test;
with Term_Test;
with Statement_Test;
with Syntaxes_Test;

package body Raptor_Test_Suite is

   use AUnit.Test_Suites;

   -- Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Test_Case_1 : aliased Basic_Test.Test_Case;
   Test_Case_2 : aliased Constants_Test.Test_Case;
   Test_Case_3 : aliased Iostreams_Test.Test_Case;
   Test_Case_4 : aliased URI_Test.Test_Case;
   Test_Case_5 : aliased Namespaces_Test.Test_Case;
   Test_Case_6 : aliased Term_Test.Test_Case;
   Test_Case_7 : aliased Statement_Test.Test_Case;
   Test_Case_8 : aliased Syntaxes_Test.Test_Case;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Case_1'Access);
      Add_Test (Result'Access, Test_Case_2'Access);
      Add_Test (Result'Access, Test_Case_3'Access);
      Add_Test (Result'Access, Test_Case_4'Access);
      Add_Test (Result'Access, Test_Case_5'Access);
      Add_Test (Result'Access, Test_Case_6'Access);
      Add_Test (Result'Access, Test_Case_7'Access);
      Add_Test (Result'Access, Test_Case_8'Access);
      return Result'Access;
   end Suite;

end Raptor_Test_Suite;
