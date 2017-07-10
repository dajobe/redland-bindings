--
--  Copyright (C) 2009-2010, AdaCore
--

package AUnit.Test_Cases.Tests_Fixtures is

   type The_Test_Case is new Test_Cases.Test_Case with record
      Is_Set_Up,
      Is_Torn_Down : Boolean := False;
   end record;
   type The_Test_Case_Access is access all The_Test_Case'Class;

   procedure Register_Tests (T : in out The_Test_Case);
   --  Register routines to be run

   function Name (T : The_Test_Case) return Test_String;
   --  Provide name identifying the test case

   procedure Set_Up (T : in out The_Test_Case);
   --  Preparation performed before each routine

   procedure Tear_Down (T : in out The_Test_Case);
   --  Cleanup performed after each routine

   function Is_Set_Up (T : The_Test_Case) return Boolean;
   --  Set up?

   function Is_Torn_Down (T : The_Test_Case) return Boolean;
   --  Torn down?

   --------------------
   --  Test Routines --
   --------------------

   procedure Fail (T : in out Test_Cases.Test_Case'Class);
   --  This routine produces a failure

   procedure Succeed (T : in out Test_Cases.Test_Case'Class);
   --  This routine does nothing, so succeeds

   procedure Double_Failure (T : in out The_Test_Case);
   --  This routine produces two failrues

   procedure Except (T : in out Test_Cases.Test_Case'Class);
   --  This routine raises an exception

end AUnit.Test_Cases.Tests_Fixtures;
