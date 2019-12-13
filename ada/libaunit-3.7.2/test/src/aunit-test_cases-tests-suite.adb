--
--  Copyright (C) 2009-2010, AdaCore
--

with AUnit.Test_Caller;

package body AUnit.Test_Cases.Tests.Suite is

   package Caller is new AUnit.Test_Caller
     (AUnit.Test_Cases.Tests.Fixture);

   function Test_Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite :=
            AUnit.Test_Suites.New_Suite;
   begin
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("(test_case) Test routines registration",
            Test_Register_Tests'Access));
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("(test_case) Test set_up phase",
            Test_Set_Up'Access));
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("(test_case) Test tear_down phase",
            Test_Torn_Down'Access));
      AUnit.Test_Suites.Add_Test
        (S,
         Caller.Create
           ("(test_case) Test run phase",
            Test_Run'Access));
      return S;
   end Test_Suite;

end AUnit.Test_Cases.Tests.Suite;
