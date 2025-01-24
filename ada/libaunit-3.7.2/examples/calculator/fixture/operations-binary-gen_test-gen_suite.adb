--
--  Copyright (C) 2008, AdaCore
--
package body Operations.Binary.Gen_Test.Gen_Suite is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Caller.Create ("Test " & Instance_Name & ".Pop", Test_Pop_Access));
      Ret.Add_Test
        (Caller.Create ("Test " & Instance_Name & ".Push", Test_Push_Access));
      Ret.Add_Test
        (Caller.Create
           ("Test " & Instance_Name & ".Execute", Test_Execute_Access));
      return Ret;
   end Suite;

end Operations.Binary.Gen_Test.Gen_Suite;
