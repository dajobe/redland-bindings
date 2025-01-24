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

   procedure Test_Addition_Failure (T : in out Test) is
      pragma Unreferenced (T);
      I1 : constant Int := 5;
      I2 : constant Int := 3;
   begin
      Assert (I1 + I2 = 9, "Test should fail this assertion, as 5+3 /= 9");
   end Test_Addition_Failure;

   procedure Test_Addition_Error (T : in out Test) is
      pragma Unreferenced (T);
      I1 : constant Int := Int'Last;
      I2 : constant Int := Int'Last;
   begin
      Assert (I1 + I2 = I1, "This raises a constraint error");
   end Test_Addition_Error;

end Math.Test;
