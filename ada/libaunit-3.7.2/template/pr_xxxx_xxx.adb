with AUnit.Assertions; use AUnit.Assertions;

--  Template for test case body.
package body PR_XXXX_XXX is

   --  Example test routine. Provide as many as are needed:
   procedure Test1 (R : in out AUnit.Test_Cases.Test_Case'Class);

   procedure Set_Up (T : in out Test_Case) is
   begin
      --  Do any necessary set ups.  If there are none,
      --  omit from both spec and body, as a default
      --  version is provided in Test_Cases.
      null;
   end Set_Up;

   procedure Tear_Down (T : in out Test_Case) is
   begin
      --  Do any necessary cleanups, so the next test
      --  has a clean environment.  If there is no
      --  cleanup, omit spec and body, as default is
      --  provided in Test_Cases.
      null;
   end Tear_Down;


   --  Example test routine. Provide as many as are needed:
   procedure Test1 (R : in out AUnit.Test_Cases.Test_Case'Class) is
   begin
      --  Do something:
      null;

      --  Test for expected conditions. Multiple assertions
      --  and actions are ok:
      Assert (True, "Indication of what failed");
   end Test1;


   --  Register test routines to call:
   procedure Register_Tests (T : in out Test_Case) is
      use Test_Cases, Test_Cases.Registration;
   begin
      --  Repeat for each test routine.
      Register_Routine (T, Test1'Access, "Description of test routine");
   end Register_Tests;

   --  Identifier of test case:
   function Name (T : Test_Case) return Message_String is
   begin
      return Format ("Test case name");
   end Name;

end PR_XXXX_XXX;
