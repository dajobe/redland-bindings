--
--  Copyright (C) 2008, AdaCore
--
with Operands.Ints; use Operands.Ints;
with Operations.Subtraction;
package Operations.Subtraction_Test_Fixture is

   procedure Set_Up
     (Op       : out Operations.Subtraction.Binary_Operation;
      Test_Op1 : out Int;
      Test_Op2 : out Int;
      Exp_Res  : out Int);

end Operations.Subtraction_Test_Fixture;
