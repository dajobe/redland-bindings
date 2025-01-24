--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Assertions; use AUnit.Assertions;

package body Operands.Ints.Test is

   procedure Test_Image (T : in out Test) is
      pragma Unreferenced (T);
      I : Int;
   begin
      I.Value := 0;
      Assert (I.Image = " 0", "Wrong image for 0");
      I.Value := 9657;
      Assert (I.Image = " 9657", "Wrong image for 9657");
      I.Value := -45879876;
      Assert (I.Image = "-45879876", "Wrong image for -45879876");
   end Test_Image;

   procedure Test_Value (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Assert (False, "test not implemented");
   end Test_Value;

   procedure Test_Set (T : in out Test) is
      pragma Unreferenced (T);
   begin
      Assert (False, "test not implemented");
   end Test_Set;

end Operands.Ints.Test;
