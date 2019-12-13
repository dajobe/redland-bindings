--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Assertions; use AUnit.Assertions;

package body Math.Test is

   procedure Set_Up (T : in out Test) is
   begin
      T.I1 := 5;
      T.I2 := 3;
   end Set_Up;

   procedure Test_Addition (T : in out Test) is
   begin
      Assert (T.I1 + T.I2 = 8, "Incorrect result after addition");
   end Test_Addition;

   procedure Test_Subtraction (T : in out Test) is
   begin
      Assert (T.I1 - T.I2 = 2, "Incorrect result after subtraction");
   end Test_Subtraction;

end Math.Test;
