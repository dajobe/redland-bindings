--
--  Copyright (C) 2009-2010, AdaCore
--

with Ada.Exceptions;
with AUnit.Assertions; use AUnit.Assertions;

package body AUnit.Test_Suites.Tests_Fixtures is

   ----------
   -- Name --
   ----------

   function Name (Test : Simple_Test_Case) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("Simple test case");
   end Name;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Test : in out Simple_Test_Case) is
      pragma Unreferenced (Test);
   begin
      null;
   end Run_Test;

   function Name (Test : TC_With_Failure) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("Test case with failure");
   end Name;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Test : in out TC_With_Failure) is
      pragma Unreferenced (Test);
   begin
      Assert (False, "A failed assertion");
   end Run_Test;

   ----------
   -- Name --
   ----------

   function Name (Test : TC_With_Two_Failures) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("Test case with 2 failures");
   end Name;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Test : in out TC_With_Two_Failures) is
      pragma Unreferenced (Test);
   begin
      if not Assert (False, "A first failure") then
         Assert (False, "A second failure");
         Assert (False, "Third failure, should not appear");
      end if;
   end Run_Test;

   ----------
   -- Name --
   ----------

   function Name (Test : TC_With_Exception) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("Test case with exception");
   end Name;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Test : in out TC_With_Exception) is
      pragma Unreferenced (Test);
   begin
      Ada.Exceptions.Raise_Exception
        (My_Exception'Identity, "A message");
   end Run_Test;

   ----------
   -- Name --
   ----------

   function Name (Test : TC_With_Setup) return Message_String is
      pragma Unreferenced (Test);
   begin
      return AUnit.Format ("Test case with set_up/tear_down defined)");
   end Name;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (Test : in out TC_With_Setup) is
   begin
      if Test.Setup then
         Test.Error := True;
      end if;

      Test.Setup := True;
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (Test : in out TC_With_Setup) is
   begin
      if not Test.Setup then
         Test.Error := True;
      end if;

      Test.Setup := False;
   end Tear_Down;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test (Test : in out TC_With_Setup) is
   begin
      Assert (Test.Setup, "Set up not done correctly");
   end Run_Test;

end AUnit.Test_Suites.Tests_Fixtures;
