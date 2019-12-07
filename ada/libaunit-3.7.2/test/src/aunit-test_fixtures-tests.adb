--
--  Copyright (C) 2009-2010, AdaCore
--

with Ada_Containers;          use Ada_Containers;
with AUnit.Assertions;    use AUnit.Assertions;
with AUnit.Options;
with AUnit.Test_Results;

with AUnit.Test_Fixtures.Tests_Fixtures;
use AUnit.Test_Fixtures.Tests_Fixtures;

package body AUnit.Test_Fixtures.Tests is

   use AUnit.Test_Fixtures.Tests_Fixtures.Caller;

   -----------------
   -- Test_Set_Up --
   -----------------

   procedure Test_Set_Up (T : in out Fixture) is
      R       : AUnit.Test_Results.Result;
      Outcome : AUnit.Status;
      Old     : constant Natural := Get_Nb_Set_Up_Called;
      pragma Unreferenced (T);

   begin
      Run (TC_Success, AUnit.Options.Default_Options, R, Outcome);
      Run (TC_Failure, AUnit.Options.Default_Options, R, Outcome);
      Assert (Get_Nb_Set_Up_Called = Old + 2,
              "Incorrect number of calls to set_up");
   end Test_Set_Up;

   ----------------------------
   -- Test_Tear_Down_Success --
   ----------------------------

   procedure Test_Tear_Down_Success (T : in out Fixture) is
      R       : AUnit.Test_Results.Result;
      Outcome : AUnit.Status;
      Old     : constant Natural := Get_Nb_Tear_Down_Called;
      pragma Unreferenced (T);

   begin
      Run (TC_Success, AUnit.Options.Default_Options, R, Outcome);
      Assert (Get_Nb_Tear_Down_Called = Old + 1,
              "Incorrect number of calls to tear_down");
      Assert (Outcome = Success,
              "Outcome value is incorrect");
      Assert (AUnit.Test_Results.Test_Count (R) = 1,
              "Incorrect number of tests reported");
      Assert (AUnit.Test_Results.Failure_Count (R) = 0,
              "Incorrect number of failures reported");
      Assert (AUnit.Test_Results.Error_Count (R) = 0,
              "Incorrect number of errors reported");
   end Test_Tear_Down_Success;

   ----------------------------
   -- Test_Tear_Down_Failure --
   ----------------------------

   procedure Test_Tear_Down_Failure (T : in out Fixture) is
      R       : AUnit.Test_Results.Result;
      Outcome : AUnit.Status;
      Old     : constant Natural := Get_Nb_Tear_Down_Called;
      pragma Unreferenced (T);

   begin
      Run (TC_Failure, AUnit.Options.Default_Options, R, Outcome);
      Assert (Get_Nb_Tear_Down_Called = Old + 1,
              "Incorrect number of calls to tear_down");
      Assert (Outcome = Failure,
              "Outcome value is incorrect");
      Assert (AUnit.Test_Results.Test_Count (R) = 1,
              "Incorrect number of tests reported");
      Assert (AUnit.Test_Results.Failure_Count (R) = 1,
              "Incorrect number of failures reported");
      Assert (AUnit.Test_Results.Error_Count (R) = 0,
              "Incorrect number of errors reported");
   end Test_Tear_Down_Failure;

   --------------------------
   -- Test_Tear_Down_Error --
   --------------------------

   procedure Test_Tear_Down_Error (T : in out Fixture) is
      R       : AUnit.Test_Results.Result;
      Outcome : AUnit.Status;
      Old     : constant Natural := Get_Nb_Tear_Down_Called;
      pragma Unreferenced (T);

   begin
      Run (TC_Error, AUnit.Options.Default_Options, R, Outcome);
      Assert (Get_Nb_Tear_Down_Called = Old + 1,
              "Incorrect number of calls to tear_down");
      Assert (Outcome = Failure,
              "Outcome value is incorrect");
      Assert (AUnit.Test_Results.Test_Count (R) = 1,
              "Incorrect number of tests reported");
      Assert (AUnit.Test_Results.Failure_Count (R) = 0,
              "Incorrect number of failures reported");
      Assert (AUnit.Test_Results.Error_Count (R) = 1,
              "Incorrect number of errors reported");
   end Test_Tear_Down_Error;

end AUnit.Test_Fixtures.Tests;
