--
--  Copyright (C) 2008, AdaCore
--
package body Operations.Subtraction_Test_Fixture is

   procedure Set_Up
     (Op       : out Operations.Subtraction.Binary_Operation;
      Test_Op1 : out Int;
      Test_Op2 : out Int;
      Exp_Res  : out Int)
   is
      pragma Unreferenced (Op);
   begin
      Test_Op1.Set (4);
      Test_Op2.Set (6);
      Exp_Res.Set (-2);
   end Set_Up;

end Operations.Subtraction_Test_Fixture;
