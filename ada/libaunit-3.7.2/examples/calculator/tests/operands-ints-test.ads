--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Test_Fixtures;

package Operands.Ints.Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Image (T : in out Test);

   procedure Test_Value (T : in out Test);

   procedure Test_Set (T : in out Test);

end Operands.Ints.Test;
