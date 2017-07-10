--
--  Copyright (C) 2008, AdaCore
--
with Shape.Tests;

package Rectangle.Tests is

   type Test is new Shape.Tests.Test with null record;

   procedure Set_Up (T : in out Test);

   procedure Test_Get_Area (T : in out Test);

end Rectangle.Tests;
