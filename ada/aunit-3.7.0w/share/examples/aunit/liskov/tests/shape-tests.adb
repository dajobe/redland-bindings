--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Assertions; use AUnit.Assertions;

package body Shape.Tests is

   --------------------
   -- Test_Set_Width --
   --------------------

   procedure Test_Set_Width (T : in out Test) is
   begin
      T.The_Shape.Set_Width (3);
      Assert
        (T.The_Shape.Width = 3,
         "Width did not return the correct value after a Set_Width");

      T.The_Shape.Set_Width (7);
      Assert
        (T.The_Shape.Width = 7,
         "Width did not return the correct value after a 2nd Set_Width");
   end Test_Set_Width;

   procedure Test_Set_Height (T : in out Test) is
   begin
      T.The_Shape.Set_Height (3);
      Assert
        (T.The_Shape.Height = 3,
         "Height did not return the correct value after a Set_Height");

      T.The_Shape.Set_Height (7);
      Assert
        (T.The_Shape.Height = 7,
         "Height did not return the correct value after a 2nd Set_Height");
   end Test_Set_Height;

end Shape.Tests;
