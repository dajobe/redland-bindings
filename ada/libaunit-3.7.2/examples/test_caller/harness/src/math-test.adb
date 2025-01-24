--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Assertions; use AUnit.Assertions;

package body Math.Test is

   procedure Test_Addition (T : in out Test) is
      pragma Unreferenced (T);
      I1 : constant Int := 5;
      I2 : constant Int := 3;
   begin
      Assert (I1 + I2 = 8, "Incorrect result after addition");
   end Test_Addition;

   procedure Test_Subtraction (T : in out Test) is
      pragma Unreferenced (T);
      I1 : constant Int := 5;
      I2 : constant Int := 3;
   begin
      Assert (I1 - I2 = 2, "Incorrect result after subtraction");
   end Test_Subtraction;

end Math.Test;
