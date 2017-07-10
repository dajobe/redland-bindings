--
--  Copyright (C) 2009-2010, AdaCore
--

with AUnit.Assertions; use AUnit.Assertions;

package body AUnit.Test_Cases.Tests is

   -------------------------
   -- Test_Register_Tests --
   -------------------------

   procedure Test_Register_Tests (T : in out Fixture)
   is
      Old_Count          : constant Count_Type :=
                             Registration.Routine_Count (T.TC);
      Routines_In_Simple : constant := 4;
   begin
      Register_Tests (T.TC);

      Assert
        (Test_Cases.Registration.Routine_Count (T.TC) =
           Old_Count + Routines_In_Simple,
         "Routine not properly registered");
   end Test_Register_Tests;

   -----------------
   -- Test_Set_Up --
   -----------------

   procedure Test_Set_Up (T : in out Fixture) is
      Was_Reset : constant Boolean := not Is_Set_Up (T.TC);
   begin
      Set_Up (T.TC);

      Assert
        (Was_Reset and Is_Set_Up (T.TC),
         "Not set up correctly");
   end Test_Set_Up;

   --------------------
   -- Test_Torn_Down --
   --------------------

   procedure Test_Torn_Down (T : in out Fixture) is
      Was_Reset : constant Boolean := not Is_Torn_Down (T.TC);
   begin
      Tear_Down (T.TC);

      Assert
        (Was_Reset and Is_Torn_Down (T.TC),
         "Not torn down correctly");
   end Test_Torn_Down;

   --------------
   -- Test_Run --
   --------------

   procedure Test_Run (T : in out Fixture) is
      Count     : constant Count_Type :=
                    Test_Cases.Registration.Routine_Count (T.TC);
      Outcome   : AUnit.Status;
      R         : Result;

   begin
      Run (T.TC'Access, AUnit.Options.Default_Options, R, Outcome);

      Assert
        (Count = 4,
         "Not enough routines in simple test case");

      Assert
        (Test_Count (R) = Count,
         "Not all requested routines were run");

      --  There are supposed to be two failed assertions for one routine
      --  in R, so we expect Count + Old_Count + 1:
      Assert
        (Success_Count (R) + Failure_Count (R) + Error_Count (R)
         = Count + 1,
         "Not all requested routines are recorded");

      Assert (Is_Torn_Down (T.TC), "Not torn down correctly");
      Assert (Success_Count (R) = 1, "Wrong success count");
      Assert (Failure_Count (R) = 3, "Wrong failures count");
      Assert (Error_Count (R) = 1, "Wrong errors count");
      Assert (Outcome = Failure, "Result flag incorrect");
   end Test_Run;

end AUnit.Test_Cases.Tests;
