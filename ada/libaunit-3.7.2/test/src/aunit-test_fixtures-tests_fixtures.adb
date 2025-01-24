--
--  Copyright (C) 2009-2010, AdaCore
--

with AUnit.Assertions; use AUnit.Assertions;

package body AUnit.Test_Fixtures.Tests_Fixtures is

   Nb_Set_Up_Called : Natural := 0;
   Nb_Tear_Down_Called : Natural := 0;

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Fix) is
   begin
      T.Set_Up_Called := T.Set_Up_Called + 1;
      Nb_Set_Up_Called := T.Set_Up_Called;
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (T : in out Fix) is
   begin
      T.Tear_Down_Called := T.Tear_Down_Called + 1;
      Nb_Tear_Down_Called := T.Tear_Down_Called;
   end Tear_Down;

   ------------------
   -- Test_Success --
   ------------------

   procedure Test_Success (T : in out Fix) is
      pragma Unreferenced (T);
   begin
      null;
   end Test_Success;

   ------------------
   -- Test_Failure --
   ------------------

   procedure Test_Failure (T : in out Fix) is
      pragma Unreferenced (T);
   begin
      Assert (False, "Failure");
   end Test_Failure;

   ----------------
   -- Test_Error --
   ----------------

   procedure Test_Error (T : in out Fix) is
      pragma Unreferenced (T);
   begin
      raise Constraint_Error;
   end Test_Error;

   --------------------------
   -- Get_Nb_Set_Up_Called --
   --------------------------

   function Get_Nb_Set_Up_Called return Natural is
   begin
      return Nb_Set_Up_Called;
   end Get_Nb_Set_Up_Called;

   -----------------------------
   -- Get_Nb_Tear_Down_Called --
   -----------------------------

   function Get_Nb_Tear_Down_Called return Natural is
   begin
      return Nb_Tear_Down_Called;
   end Get_Nb_Tear_Down_Called;

end AUnit.Test_Fixtures.Tests_Fixtures;
