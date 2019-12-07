--
--  Copyright (C) 2009-2010, AdaCore
--

with AUnit.Options;      use AUnit.Options;
with AUnit.Assertions;   use AUnit.Assertions;
with AUnit.Test_Results; use AUnit.Test_Results;
with AUnit.Time_Measure; use AUnit.Time_Measure;

with AUnit.Test_Suites.Tests_Fixtures; use AUnit.Test_Suites.Tests_Fixtures;

package body AUnit.Test_Suites.Tests is

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (Test : in out Fixture) is
   begin
      AUnit.Test_Results.Clear (Test.Res);
      Test.Suite := AUnit.Test_Suites.New_Suite;
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (Test : in out Fixture) is
--        --  ??? incompatible with zfp. Should we remove it ?
--        procedure Free is new Ada.Unchecked_Deallocation
--          (Access_Test_Suite, Test_Suite);
      pragma Unreferenced (Test);
   begin
      null;
--        Free (Test.Suite);
   end Tear_Down;

   ------------------------
   -- Test_Add_Test_Case --
   ------------------------

   procedure Test_Add_Test_Case (T : in out Fixture) is
   begin
      Assert (Test_Lists.Is_Empty (T.Suite.Tests),
              "Suite is not empty when initialized");
      AUnit.Test_Suites.Add_Test (T.Suite, A_Simple_Test_Case'Access);
      Assert (Test_Lists.Length (T.Suite.Tests) = 1,
              "Suite length after inserting a test case is not 1");
   end Test_Add_Test_Case;

   --------------------
   -- Test_Run_Empty --
   --------------------

   procedure Test_Run_Empty (T : in out Fixture) is
      Outcome : AUnit.Status;
      Option  : constant AUnit.Options.AUnit_Options := Default_Options;
   begin
      Run (T.Suite, Option, T.Res, Outcome);

      Assert (Successful (T.Res), "Suite did not report success correctly");
      Assert (Test_Count (T.Res) = 0, "Wrong number of tests recorded");
      Assert (Outcome = Success, "Result flag incorrect");
   end Test_Run_Empty;

   ---------------------------
   -- Test_Run_With_Success --
   ---------------------------

   procedure Test_Run_With_Success (T : in out Fixture) is
      Outcome : AUnit.Status;
      Option  : constant AUnit_Options := Default_Options;
   begin
      AUnit.Test_Suites.Add_Test (T.Suite, A_Simple_Test_Case'Access);
      Run (T.Suite, Option, T.Res, Outcome);

      Assert (Successful (T.Res), "Suite did not report success correctly");
      Assert (Success_Count (T.Res) = 1,
              "Number of reported successes is wrong");
      Assert (Failure_Count (T.Res) = 0,
              "Number of reported failures is wrong");
      Assert (Error_Count (T.Res) = 0,
              "Number of reported errors is wrong");
      Assert (Test_Count (T.Res) = 1, "Wrong number of tests recorded");
      Assert (Outcome = Success, "Result flag incorrect");
   end Test_Run_With_Success;

   ---------------------------
   -- Test_Run_With_Failure --
   ---------------------------

   procedure Test_Run_With_Failure (T : in out Fixture) is
      Outcome : AUnit.Status;
      Option  : constant AUnit_Options := Default_Options;
   begin
      AUnit.Test_Suites.Add_Test (T.Suite, A_TC_With_Failure'Access);
      Run (T.Suite, Option, T.Res, Outcome);

      Assert (not Successful (T.Res),
              "Suite did not report success correctly");
      Assert (Success_Count (T.Res) = 0,
              "Number of reported successes is wrong");
      Assert (Failure_Count (T.Res) = 1,
              "Number of reported failures is wrong");
      Assert (Error_Count (T.Res) = 0,
              "Number of reported errors is wrong");
      Assert (Test_Count (T.Res) = 1, "Wrong number of tests recorded");
      Assert (Outcome = Failure, "Result flag incorrect");
   end Test_Run_With_Failure;

   -----------------------------
   -- Test_Run_With_Exception --
   -----------------------------

   procedure Test_Run_With_Exception (T : in out Fixture) is
      Outcome : AUnit.Status;
      Option  : constant AUnit_Options := Default_Options;
   begin
      AUnit.Test_Suites.Add_Test (T.Suite, A_TC_With_Exception'Access);
      Run (T.Suite, Option, T.Res, Outcome);

      Assert (not Successful (T.Res),
              "Suite did not report success correctly");
      Assert (Success_Count (T.Res) = 0,
              "Number of reported successes is wrong");
      Assert (Failure_Count (T.Res) = 0,
              "Number of reported failures is wrong");
      Assert (Error_Count (T.Res) = 1,
              "Number of reported errors is wrong");
      Assert (Test_Count (T.Res) = 1, "Wrong number of tests recorded");
      Assert (Outcome = Failure, "Result flag incorrect");
   end Test_Run_With_Exception;

   -----------------------------
   -- Test_Run_With_Exception --
   -----------------------------

   procedure Test_Run_With_All (T : in out Fixture) is
      Outcome : AUnit.Status;
      Option  : constant AUnit_Options := Default_Options;
   begin
      AUnit.Test_Suites.Add_Test (T.Suite, A_Simple_Test_Case'Access);
      AUnit.Test_Suites.Add_Test (T.Suite, A_TC_With_Two_Failures'Access);
      AUnit.Test_Suites.Add_Test (T.Suite, A_TC_With_Exception'Access);
      Run (T.Suite, Option, T.Res, Outcome);

      Assert (not Successful (T.Res),
              "Suite did not report success correctly");
      Assert (Success_Count (T.Res) = 1,
              "Number of reported successes is wrong");
      Assert (Failure_Count (T.Res) = 2,
              "Number of reported failures is wrong");
      Assert (Error_Count (T.Res) = 1,
              "Number of reported errors is wrong");
      Assert (Test_Count (T.Res) = 3, "Wrong number of tests recorded");
      Assert (Outcome = Failure, "Result flag incorrect");

      declare
         List : Result_Lists.List;
         Elem : Test_Result;
      begin
         Successes (T.Res, List);
         Assert (Result_Lists.Length (List) = 1,
                 "Unexpected number of successful results");
         Elem := Result_Lists.First_Element (List);
         Assert (Elem.Test_Name.all = "Simple test case",
                 "Incorrect test name in result: '" &
                 Elem.Test_Name.all & "'");

         --  Do not use Elem.Routine_Name.all as test result string, as this
         --  would be elaborated even in the normal case where null is
         --  expected.

         Assert (Elem.Routine_Name = null,
                 "Incorrect routine name for result: expected null");
         Assert (Elem.Failure = null,
                 "Unexpected failure value for a successful run");
         Assert (Elem.Error = null,
                 "Unexpected error value for a successful run");
         Assert (Elem.Elapsed = Null_Time,
                 "Unexpected elapsed value with run option set to No_Time");
         Result_Lists.Clear (List);

         Failures (T.Res, List);
         Assert (Result_Lists.Length (List) = 2,
                 "Unexpected number of failure results");
         Elem := Result_Lists.First_Element (List);
         Assert (Elem.Test_Name.all = "Test case with 2 failures",
                 "Incorrect test name for result: '" &
                 Elem.Test_Name.all & "'");
         Assert (Elem.Routine_Name = null,
                 "Incorrect routine name for result: expected null");
         Assert (Elem.Failure /= null,
                 "Unexpected failure value for a failed run");
         Assert (Elem.Error = null,
                 "Unexpected error value for a failed run");
         Assert (Elem.Elapsed = Null_Time,
                 "Unexpected elapsed value with run option set to No_Time");
         Assert (Elem.Failure.Message.all = "A first failure",
                 "Incorrect message reported in Failure");
         Assert (Elem.Failure.Source_Name.all,
                 "aunit-test_suites-tests_fixtures.adb",
                 "Incorrect filename reported in Failure");
         Result_Lists.Clear (List);

         Errors (T.Res, List);
         Assert (Result_Lists.Length (List) = 1,
                 "Unexpected number of error results");
         Elem := Result_Lists.First_Element (List);
         Assert (Elem.Test_Name.all = "Test case with exception",
                 "Incorrect test name for result: '" &
                 Elem.Test_Name.all & "'");
         Assert (Elem.Routine_Name = null,
                 "Incorrect routine name for result: expected null");
         Assert (Elem.Failure = null,
                 "Unexpected failure value for a run with exception raised");
         Assert (Elem.Error /= null,
                 "Unexpected error value for a run with exception raised");
         Assert (Elem.Elapsed = Null_Time,
                 "Unexpected elapsed value with run option set to No_Time");
         Assert (Elem.Error.Exception_Name.all =
                   "AUNIT.TEST_SUITES.TESTS_FIXTURES.MY_EXCEPTION"
                 or else Elem.Error.Exception_Name.all =
                   "Unexpected exception in zfp profile",
                 "Exeption name is incorrect in error: '" &
                 Elem.Error.Exception_Name.all & "'");

         --  Incompatible with certexceptions
         --  Assert (Elem.Error.Exception_Message.all = "A message",
         --  "Exception message not correctly set: " &
         --  Elem.Error.Exception_Message.all);
      end;
   end Test_Run_With_All;

   -------------------------
   -- Test_Run_With_Setup --
   -------------------------

   procedure Test_Run_With_Setup (T : in out Fixture) is
      Outcome : AUnit.Status;
      Option  : constant AUnit_Options := Default_Options;
   begin
      AUnit.Test_Suites.Add_Test (T.Suite, A_TC_With_Setup'Access);
      Run (T.Suite, Option, T.Res, Outcome);

      Assert (Successful (T.Res),
              "Suite did not run successfully: setup not called");
      Assert (A_TC_With_Setup.Setup = False,
              "Tear down not called");
      Assert (A_TC_With_Setup.Error = False,
              "Tear down did not receive the expected value");
   end Test_Run_With_Setup;

end AUnit.Test_Suites.Tests;
