--
--  Copyright (C) 2009-2010, AdaCore
--

package AUnit.Test_Fixtures.Tests is

   type Fixture is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Set_Up (T : in out Fixture);
   --  Test that Set_Up is correctly called when running a test, and that the
   --  same fixture object is used for different tests.

   procedure Test_Tear_Down_Success (T : in out Fixture);
   --  Test that Tear_Down is correctly called when running a test that
   --  succeeds, and that test result is correct.

   procedure Test_Tear_Down_Failure (T : in out Fixture);
   --  Test that Tear_Down is correctly called when running a test that
   --  fails, and that test result is correct.

   procedure Test_Tear_Down_Error (T : in out Fixture);
   --  Test that Tear_Down is correctly called when running a test with
   --  an error, and that test result is correct.

end AUnit.Test_Fixtures.Tests;
