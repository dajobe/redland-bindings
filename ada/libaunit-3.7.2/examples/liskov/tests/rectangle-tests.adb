--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Assertions;   use AUnit.Assertions;

with Shape;

package body Rectangle.Tests is

   -----------------
   -- Set_Up_Case --
   -----------------

   Local_Rectangle : aliased Rectangle_Type;
   procedure Set_Up (T : in out Test) is
   begin
      T.The_Shape := Local_Rectangle'Access;
   end Set_Up;

   -------------------
   -- Test_Get_Area --
   -------------------

   procedure Test_Get_Area (T : in out Test) is
   begin
      Shape.Set_Width (T.The_Shape.all, 3);
      Shape.Set_Height (T.The_Shape.all, 5);
      Assert (Shape.Area (T.The_Shape.all) = 15,
              "Wrong area returned for object rectangle");
   end Test_Get_Area;

end Rectangle.Tests;
