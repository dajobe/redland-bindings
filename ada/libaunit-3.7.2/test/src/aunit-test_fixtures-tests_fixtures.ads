--
--  Copyright (C) 2009-2010, AdaCore
--

with AUnit.Test_Caller;

package AUnit.Test_Fixtures.Tests_Fixtures is

   type Fix is new AUnit.Test_Fixtures.Test_Fixture with record
      Set_Up_Called    : Natural := 0;
      Tear_Down_Called : Natural := 0;
   end record;

   procedure Set_Up (T : in out Fix);
   procedure Tear_Down (T : in out Fix);

   procedure Test_Success (T : in out Fix);
   procedure Test_Failure (T : in out Fix);
   procedure Test_Error (T : in out Fix);

   package Caller is new AUnit.Test_Caller (Fix);

   TC_Success : constant Caller.Test_Case_Access :=
                  Caller.Create ("Test Success", Test_Success'Access);
   TC_Failure : constant Caller.Test_Case_Access :=
                  Caller.Create ("Test Failure", Test_Failure'Access);
   TC_Error   : constant Caller.Test_Case_Access :=
                  Caller.Create ("Test Error", Test_Error'Access);

   function Get_Nb_Set_Up_Called return Natural;
   function Get_Nb_Tear_Down_Called return Natural;

end AUnit.Test_Fixtures.Tests_Fixtures;
