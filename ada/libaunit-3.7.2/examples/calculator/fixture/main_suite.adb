--
--  Copyright (C) 2008, AdaCore
--
with Stack.Test.Suite;
with Operations.Addition.Test.Suite;
with Operations.Subtraction.Test.Suite;
with Operands.Ints.Test.Suite;

package body Main_Suite is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (Stack.Test.Suite.Suite);
      Ret.Add_Test (Operations.Addition.Test.Suite.Suite);
      Ret.Add_Test (Operations.Subtraction.Test.Suite.Suite);
      Ret.Add_Test (Operands.Ints.Test.Suite.Suite);
      return Ret;
   end Suite;

end Main_Suite;
