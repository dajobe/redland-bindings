--
--  Copyright (C) 2008, AdaCore
--

with AUnit.Test_Caller;

package body Square.Tests.Suite is

   package Runner is new AUnit.Test_Caller
     (Square.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Test_Width  : aliased Runner.Test_Case;
   Test_Height : aliased Runner.Test_Case;
   Test_Area   : aliased Runner.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Runner.Create (Test_Width,
                     "Square : Test width",
                     Test_Set_Width'Access);
      Runner.Create (Test_Height,
                     "Square : Test height",
                     Test_Set_Height'Access);
      Runner.Create (Test_Area,
                     "Square : Test area",
                     Test_Get_Area'Access);
      Result.Add_Test (Test_Width'Access);
      Result.Add_Test (Test_Height'Access);
      Result.Add_Test (Test_Area'Access);

      return Result'Access;
   end Suite;

end Square.Tests.Suite;
