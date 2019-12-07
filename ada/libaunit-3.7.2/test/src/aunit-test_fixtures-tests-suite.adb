--
--  Copyright (C) 2009-2010, AdaCore
--

with AUnit.Test_Caller;

package body AUnit.Test_Fixtures.Tests.Suite is

   package Caller is new AUnit.Test_Caller
     (AUnit.Test_Fixtures.Tests.Fixture);

   function Test_Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite :=
            AUnit.Test_Suites.New_Suite;
   begin
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("(test_fixture) Test Set_Up call",
            Test_Set_Up'Access));
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("(test_fixture) Test Tear_Down call (the called test is success)",
            Test_Tear_Down_Success'Access));
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("(test_fixture) Test Tear_Down call (the called test is failure)",
            Test_Tear_Down_Failure'Access));
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("(test_fixture) Test Tear_Down call (the called test is error)",
            Test_Tear_Down_Error'Access));
      return S;
   end Test_Suite;

end AUnit.Test_Fixtures.Tests.Suite;
