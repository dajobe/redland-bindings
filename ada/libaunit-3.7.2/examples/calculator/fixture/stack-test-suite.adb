--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Test_Caller;

package body Stack.Test.Suite is

   package Caller is new AUnit.Test_Caller (Stack.Test.Test);

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Caller.Create ("Test Stack.Push", Test_Push'Access));
      Ret.Add_Test
        (Caller.Create ("Test Stack.Pop", Test_Pop'Access));
      Ret.Add_Test
        (Caller.Create ("Test Stack.Length", Test_Length'Access));
      Ret.Add_Test
        (Caller.Create ("Test Stack.Top", Test_Top'Access));
      Ret.Add_Test
        (Caller.Create ("Test Stack.Next_To_Top", Test_Next_To_Top'Access));
      return Ret;
   end Suite;

end Stack.Test.Suite;
