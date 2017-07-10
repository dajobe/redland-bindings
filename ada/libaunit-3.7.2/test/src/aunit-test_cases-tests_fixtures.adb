--
--  Copyright (C) 2009-2010, AdaCore
--

with AUnit.Assertions; use AUnit.Assertions;

package body AUnit.Test_Cases.Tests_Fixtures is

   procedure Double_Failure_Wrapper (T : in out The_Test_Case'Class);

   use AUnit.Test_Cases.Registration;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out The_Test_Case) is
   begin
      T.Is_Set_Up := True;
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (T : in out The_Test_Case) is
   begin
      T.Is_Torn_Down := True;
   end Tear_Down;

   -------------
   -- Succeed --
   -------------

   procedure Succeed (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      null;
   end Succeed;

   ----------
   -- Fail --
   ----------

   procedure Fail (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (False, "Failure test failed");
   end Fail;

   ----------------------------
   -- Double_Failure_Wrapper --
   ----------------------------

   procedure Double_Failure_Wrapper (T : in out The_Test_Case'Class) is
   begin
      Double_Failure (T);
   end Double_Failure_Wrapper;

   --------------------
   -- Double_Failure --
   --------------------

   procedure Double_Failure (T : in out The_Test_Case) is
      Dummy : Boolean;
      pragma Unreferenced (T, Dummy);
   begin
      --  Fail two assertions. Will be checked in Test_Test_Case.Test_Run
      Dummy := Assert (False, "first failure");
      Assert (False, "second failure");
   end Double_Failure;

   ------------
   -- Except --
   ------------

   procedure Except (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      raise Constraint_Error;
   end Except;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out The_Test_Case) is
      package Register_Specific is
        new AUnit.Test_Cases.Specific_Test_Case_Registration
          (The_Test_Case);
      use Register_Specific;
   begin

      Register_Routine
        (T, Succeed'Access, "Success Test");

      Register_Routine
        (T, Fail'Access, "Failure Test");

      Register_Wrapper
        (T,
         Double_Failure_Wrapper'Access,
         "Multiple assertion failures");

      Register_Routine
        (T, Except'Access, "Exception Test");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   function Name (T : The_Test_Case) return Test_String is
      pragma Unreferenced (T);
   begin
      return Format ("Dummy Test Case");
   end Name;

   ---------------
   -- Is_Set_Up --
   ---------------

   function Is_Set_Up (T : The_Test_Case) return Boolean is
   begin
      return T.Is_Set_Up;
   end Is_Set_Up;

   ------------------
   -- Is_Torn_Down --
   ------------------

   function Is_Torn_Down (T : The_Test_Case) return Boolean is
   begin
      return T.Is_Torn_Down;
   end Is_Torn_Down;

end AUnit.Test_Cases.Tests_Fixtures;
