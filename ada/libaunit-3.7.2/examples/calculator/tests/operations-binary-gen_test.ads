--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Test_Fixtures;

generic

   with procedure Set_Up
     (The_Op    : out Binary_Operation;
      Test_Op1  : out T;
      Test_Op2  : out T;
      Exp_Res   : out T_Ret);

package Operations.Binary.Gen_Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with private;

   procedure Set_Up (T : in out Test);
   procedure Tear_Down (T : in out Test);

   procedure Test_Pop (T : in out Test);
   procedure Test_Push (T : in out Test);
   procedure Test_Execute (T : in out Test);

private

   type Test is new AUnit.Test_Fixtures.Test_Fixture with record
      Op : Operations.Binary.Binary_Operation;
      Test_Op1 : T;
      Test_Op2 : T;
      Exp_Res  : T_Ret;
   end record;

end Operations.Binary.Gen_Test;
