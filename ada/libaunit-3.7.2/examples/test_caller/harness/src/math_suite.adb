--
--  Copyright (C) 2008, AdaCore
--
with Math.Test;         use Math.Test;
with AUnit.Test_Caller;

package body Math_Suite is

   package Caller is new AUnit.Test_Caller (Math.Test.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := AUnit.Test_Suites.New_Suite;
   begin
      Ret.Add_Test
        (Caller.Create ("Test addition", Test_Addition'Access));
      Ret.Add_Test
        (Caller.Create ("Test subtraction", Test_Subtraction'Access));
      return Ret;
   end Suite;

end Math_Suite;
