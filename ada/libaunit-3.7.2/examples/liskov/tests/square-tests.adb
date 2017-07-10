--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Assertions;   use AUnit.Assertions;

package body Square.Tests is

   -----------------
   -- Set_Up_Case --
   -----------------

   Local_Square : aliased Square_Type;

   procedure Set_Up (T : in out Test) is
   begin
      T.The_Shape := Local_Square'Access;
   end Set_Up;

   -------------------
   -- Test_Get_Area --
   -------------------

   procedure Test_Get_Area (T : in out Test) is
   begin
      T.The_Shape.Set_Width (3);
      Assert (T.The_Shape.Area = 9,
              "Wrong area returned for object square");
      T.The_Shape.Set_Height (5);
      Assert (T.The_Shape.Area = 25,
              "Wrong area returned for object square");
   end Test_Get_Area;

end Square.Tests;
