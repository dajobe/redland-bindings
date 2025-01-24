--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Test_Fixtures;

package Shape.Tests is

   type Test is abstract new AUnit.Test_Fixtures.Test_Fixture
   with record
      The_Shape : Shape_Access;
   end record;

   procedure Test_Set_Width (T : in out Test);
   procedure Test_Set_Height (T : in out Test);
   procedure Test_Get_Area (T : in out Test) is abstract;

end Shape.Tests;
