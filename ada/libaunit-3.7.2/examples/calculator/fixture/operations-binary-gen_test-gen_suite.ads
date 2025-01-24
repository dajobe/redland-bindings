--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Test_Suites; use AUnit.Test_Suites;
with AUnit.Test_Caller;

generic
   Instance_Name : String;
package Operations.Binary.Gen_Test.Gen_Suite is

   function Suite return Access_Test_Suite;

private

   package Caller is new AUnit.Test_Caller (Operations.Binary.Gen_Test.Test);
   Test_Pop_Access : constant Caller.Test_Method := Test_Pop'Access;
   Test_Push_Access : constant Caller.Test_Method := Test_Push'Access;
   Test_Execute_Access : constant Caller.Test_Method := Test_Execute'Access;

end Operations.Binary.Gen_Test.Gen_Suite;
