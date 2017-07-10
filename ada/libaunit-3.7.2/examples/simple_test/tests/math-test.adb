--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Assertions; use AUnit.Assertions;

package body Math.Test is

   function Name (T : Test) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Test Math package");
   end Name;

   procedure Run_Test (T : in out Test) is
      pragma Unreferenced (T);
      I1 : constant Int := 5;
      I2 : constant Int := 3;
   begin
      Assert (I1 + I2 = 8, "Incorrect result after addition");
      Assert (I1 - I2 = 2, "Incorrect result after subtraction");
   end Run_Test;

end Math.Test;
