--
--  Copyright (C) 2009-2010, AdaCore
--

with AUnit.Test_Fixtures;
with AUnit.Test_Results;
with AUnit.Test_Suites;

package AUnit.Test_Suites.Tests is

   type Fixture is new AUnit.Test_Fixtures.Test_Fixture with record
      Suite : AUnit.Test_Suites.Access_Test_Suite;
      Res   : AUnit.Test_Results.Result;
   end record;

   procedure Set_Up (Test : in out Fixture);
   procedure Tear_Down (Test : in out Fixture);

   procedure Test_Add_Test_Case (T : in out Fixture);
   procedure Test_Run_Empty (T : in out Fixture);
   procedure Test_Run_With_Success (T : in out Fixture);
   procedure Test_Run_With_Failure (T : in out Fixture);
   procedure Test_Run_With_Exception (T : in out Fixture);
   procedure Test_Run_With_All (T : in out Fixture);
   procedure Test_Run_With_Setup (T : in out Fixture);

end AUnit.Test_Suites.Tests;
