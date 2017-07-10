--
--  Copyright (C) 2008, AdaCore
--
with Rectangle.Tests;

package Square.Tests is

   type Test is new Rectangle.Tests.Test with
     null record;

   procedure Set_Up (T : in out Test);

   procedure Test_Get_Area (T : in out Test);

end Square.Tests;
