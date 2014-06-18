--
--  Copyright (C) 2009-2010, AdaCore
--

with AUnit.Test_Caller;

package body AUnit.Test_Suites.Tests.Suite is

   package Caller is new AUnit.Test_Caller
     (AUnit.Test_Suites.Tests.Fixture);

   function Test_Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite :=
            AUnit.Test_Suites.New_Suite;
   begin
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("(suite) Add test case",
            Test_Add_Test_Case'Access));
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("(suite) Run empty suite",
            Test_Run_Empty'Access));
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("(suite) Run suite with a successful test",
            Test_Run_With_Success'Access));
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("(suite) Run suite with a failing test",
            Test_Run_With_Failure'Access));
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("(suite) Run suite with a test raising an exception",
            Test_Run_With_Exception'Access));
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("(suite) Run suite with various tests",
            Test_Run_With_All'Access));
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("(suite) Verify Set_Up/Tear_Down are called",
            Test_Run_With_Setup'Access));
      return S;
   end Test_Suite;

end AUnit.Test_Suites.Tests.Suite;
