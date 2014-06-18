--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Test_Caller;

package body Operands.Ints.Test.Suite is

   package Caller is new AUnit.Test_Caller (Operands.Ints.Test.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Caller.Create ("Test Operands.Ints.Image", Test_Image'Access));
      Ret.Add_Test
        (Caller.Create ("Test Operands.Ints.Value", Test_Value'Access));
      Ret.Add_Test
        (Caller.Create ("Test Operands.Ints.Set", Test_Set'Access));
      return Ret;
   end Suite;

end Operands.Ints.Test.Suite;
