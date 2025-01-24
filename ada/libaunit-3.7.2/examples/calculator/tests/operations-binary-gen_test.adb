--
--  Copyright (C) 2008, AdaCore
--
with Ada.Exceptions;
with System.Assertions;
with AUnit.Assertions; use AUnit.Assertions;
with Stack;            use Stack;
package body Operations.Binary.Gen_Test is

   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Test) is
   begin
      Set_Up (T.Op, T.Test_Op1, T.Test_Op2, T.Exp_Res);
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Make sure the stack is empty after each test.
      while Stack.Length > 0 loop
         declare
            Op : constant Operands.Operand'Class := Stack.Pop;
            pragma Unreferenced (Op);
         begin
            null;
         end;
      end loop;
   end Tear_Down;

   --------------
   -- Test_Pop --
   --------------

   procedure Test_Pop (T : in out Test) is
   begin
      begin
         Pop (T.Op);
         Assert (False, "stack is empty, it should have raised an exception");
      exception
         when System.Assertions.Assert_Failure =>
            --  Precondition failed. OK
            null;
         when E : others =>
            Assert (False, "Wrong exception raised: " &
                    Ada.Exceptions.Exception_Name (E));
      end;

      Stack.Push (T.Test_Op1);
      Stack.Push (T.Test_Op2);
      Pop (T.Op);
      Assert (Stack.Length = 0, "Wrong pop operation");
      Assert (T.Op.Op1 = T.Test_Op1, "Wrong first value poped");
      Assert (T.Op.Op2 = T.Test_Op2, "Wrong 2nd value poped");
   end Test_Pop;

   ---------------
   -- Test_Push --
   ---------------

   procedure Test_Push (T : in out Test) is
   begin
      T.Op.Res := T.Exp_Res;
      T.Op.Push;
      Assert (Stack.Length = 1, "Wrong push on stack");
      Assert (Stack.Top = Operands.Operand'Class (T.Exp_Res),
              "Wrong value pushed");
      for J in 2 .. Stack.Max_Stack_Size loop
         Stack.Push (T.Test_Op1);
      end loop;

      begin
         T.Op.Push;
         Assert (False, "stack is full, it should have raised an exception");
      exception
         when System.Assertions.Assert_Failure =>
            null; --  Expected
         when E : others =>
            Assert (False, "Wrong exception raised: " &
                    Ada.Exceptions.Exception_Name (E));
      end;
   end Test_Push;

   ------------------
   -- Test_Execute --
   ------------------

   procedure Test_Execute (T : in out Test) is
   begin
      T.Op.Op1 := T.Test_Op1;
      T.Op.Op2 := T.Test_Op2;
      T.Op.Execute;
      Assert (T.Op.Res = T.Exp_Res, "Incorrect result set after Execute");
   end Test_Execute;

end Operations.Binary.Gen_Test;
