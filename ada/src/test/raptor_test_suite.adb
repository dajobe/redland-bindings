with Basic_Test;

package body Raptor_Test_Suite is

   use AUnit.Test_Suites;

   -- Statically allocate test suite:
   Result : aliased Test_Suite;

   --  Statically allocate test cases:
   Test_Case_1 : aliased Basic_Test.Test_Case;

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Case_1'Access);
      return Result'Access;
   end Suite;

end Raptor_Test_Suite;
