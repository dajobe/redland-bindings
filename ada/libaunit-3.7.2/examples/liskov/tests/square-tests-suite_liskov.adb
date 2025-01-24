--
--  Copyright (C) 2008, AdaCore
--
with Ada.Unchecked_Conversion;
with AUnit.Test_Caller;
with Rectangle.Tests;

package body Square.Tests.Suite_Liskov is

   package Runner is new AUnit.Test_Caller
     (Square.Tests.Test);
   package Rectangle_Runner is new AUnit.Test_Caller
     (Rectangle.Tests.Test);

   Result : aliased AUnit.Test_Suites.Test_Suite;

   Test_Width  : aliased Runner.Test_Case;
   Test_Height : aliased Runner.Test_Case;
   Test_Area   : aliased Runner.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      function Convert is new Ada.Unchecked_Conversion
        (Rectangle_Runner.Test_Method, Runner.Test_Method);
   begin
      Runner.Create
        (Test_Width,
         "Square as Rectangle (liskov) : Test width",
         Convert
           (Rectangle_Runner.Test_Method'
              (Rectangle.Tests.Test_Set_Width'Access)));
      Runner.Create
        (Test_Height,
         "Square as Rectangle (liskov) : Test height",
         Convert
           (Rectangle_Runner.Test_Method'
              (Rectangle.Tests.Test_Set_Height'Access)));
      Runner.Create
        (Test_Area,
         "Square as Rectangle (liskov) : Test area",
         Convert
           (Rectangle_Runner.Test_Method'
              (Rectangle.Tests.Test_Get_Area'Access)));
      Result.Add_Test (Test_Width'Access);
      Result.Add_Test (Test_Height'Access);
      Result.Add_Test (Test_Area'Access);

      return Result'Access;
   end Suite;

end Square.Tests.Suite_Liskov;
