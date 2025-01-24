--
--  Copyright (C) 2008, AdaCore
--
with AUnit;
with AUnit.Test_Fixtures;

package Math.Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Addition (T : in out Test);
   procedure Test_Subtraction (T : in out Test);

   procedure Test_Addition_Failure (T : in out Test);
   --  This test will do a failed assertion

   procedure Test_Addition_Error (T : in out Test);
   --  This test will raise an exception

end Math.Test;
