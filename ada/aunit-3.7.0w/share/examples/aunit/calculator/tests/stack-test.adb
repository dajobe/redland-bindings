--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Assertions; use AUnit.Assertions;
with Ada.Exceptions;
with Operands.Ints;    use Operands.Ints;

package body Stack.Test is

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (T : in out Test) is
      pragma Unreferenced (T);
   begin
      --  Make sure the stack is empty after each test.
      while Stack.The_Stack_Index > 0 loop
         Free (Stack.The_Stack (Stack.The_Stack_Index));
         The_Stack_Index := The_Stack_Index - 1;
      end loop;
   end Tear_Down;

   ---------------
   -- Test_Push --
   ---------------

   procedure Test_Push (T : in out Test) is
      pragma Unreferenced (T);
      I1, I2 : Int;
   begin
      I1.Set (3);
      I2.Set (-4);

      --  Test single push
      Stack.Push (I1);
      Assert (Stack.The_Stack_Index = 1, "Wrong stack index after push");
      Assert (Stack.The_Stack (1).all = Operand'Class (I1),
              "Wrong value pushed on the stack");

      --  Test second push
      Stack.Push (I2);
      Assert (Stack.The_Stack_Index = 2, "Wrong stack index after 2nd push");
      Assert (Stack.The_Stack (1).all = Operand'Class (I1)
              and then Stack.The_Stack (2).all = Operand'Class (I2),
              "Wrong value order after two pushes on the stack");

      --  Test overflow
      begin
         for J in The_Stack_Index .. The_Stack'Last loop
            Stack.Push (I2);
         end loop;

         Assert (False, "Stack_Overflow should have been raised");
      exception
         when Stack.Stack_Overflow =>
            null;
         when E : others =>
            Assert (False, "Wrong exception raised : " &
                    Ada.Exceptions.Exception_Name (E));
      end;
   end Test_Push;

   --------------
   -- Test_Pop --
   --------------

   procedure Test_Pop (T : in out Test) is
      pragma Unreferenced (T);
      I1, I2 : Int;
      I3     : Int;
      pragma Unreferenced (I3);
   begin
      I1.Set (3);
      I2.Set (-4);

      The_Stack (1) := new Operand'Class'(Operand'Class (I1));
      The_Stack (2) := new Operand'Class'(Operand'Class (I2));
      The_Stack_Index := 2;

      Assert (Stack.Pop = Operand'Class (I2),
              "Wrong value poped with 2 values on the stack");
      Assert (The_Stack_Index = 1,
              "Wrong stack index after pop");
      Assert (Stack.Pop = Operand'Class (I1),
              "Wrong value poped with 1 value on the stack");
      Assert (The_Stack_Index = 0,
              "Wrong stack index after 2nd pop");
      begin
         I3 := Int (Stack.Pop);
         Assert (False, "Stack_Empty should have been raised");

      exception
         when Stack.Stack_Empty =>
            --  Expected exception
            null;
         when E : others =>
            Assert (False, "Wrong exception raised : " &
                    Ada.Exceptions.Exception_Name (E));
      end;
   end Test_Pop;

   -----------------
   -- Test_Length --
   -----------------

   procedure Test_Length (T : in out Test) is
      pragma Unreferenced (T);
      I1, I2 : Int;
      I3     : Int;
      pragma Unreferenced (I3);
   begin
      I1.Set (3);
      I2.Set (-4);

      Stack.Push (I1);
      Assert (Stack.Length = 1, "Wrong length after 1 push");
      Stack.Push (I2);
      Assert (Stack.Length = 2, "Wrong length after 2 push");
      I3 := Int (Stack.Pop);
      Assert (Stack.Length = 1, "Wrong length after 2 pushes and 1 pop");

      begin
         for J in 1 .. Stack.The_Stack'Last loop
            Stack.Push (I2);
         end loop;
      exception
         when Stack_Overflow =>
            Assert (Stack.Length = Natural (Stack.The_Stack'Last),
                    "Stack.Length incorrect after Stack_Overflow exception");
      end;

      begin
         for J in 0 .. Stack.The_Stack'Last loop
            I3 := Int (Stack.Pop);
         end loop;
      exception
         when Stack_Empty =>
            Assert (Stack.Length = 0,
                    "Stack.Length incorrect after Stack_Empty exception");
      end;
   end Test_Length;

   --------------
   -- Test_Top --
   --------------

   procedure Test_Top (T : in out Test) is
      pragma Unreferenced (T);
      I1, I2 : Int;
      I3     : Int;
      pragma Unreferenced (I3);
   begin
      I1.Set (3);
      I2.Set (-4);

      Stack.Push (I1);
      Assert (Stack.Top = Operand'Class (I1),
              "Wrong value returned by Top after 1 push");
      Assert (Stack.Length = 1, "Top modified the length");
      Stack.Push (I2);
      Assert (Stack.Top = Operand'Class (I2),
              "Wrong value returned by Top after 2 pushes");
      Assert (Stack.Length = 2, "Top modified the length");
      I3 := Int (Stack.Pop);
      I3 := Int (Stack.Pop);
      begin
         I3 := Int (Stack.Top);
         Assert
           (False,
            "Top should have raised Emtpy_Stack when the stack is empty");
      exception
         when Stack.Stack_Empty =>
            null;
         when E : others =>
            Assert (False, "Wrong exception raised : " &
                    Ada.Exceptions.Exception_Name (E));
      end;
   end Test_Top;

   ----------------------
   -- Test_Next_To_Top --
   ----------------------

   procedure Test_Next_To_Top (T : in out Test) is
      pragma Unreferenced (T);
      I1, I2 : Int;
      I3     : Int;
      pragma Unreferenced (I3);
   begin
      I1.Set (3);
      I2.Set (-4);

      Stack.Push (I1);
      Stack.Push (I2);
      Assert (Stack.Next_To_Top = Operand'Class (I1),
              "Wrong value returned by Next_To_Top after 2 pushes");
      Assert (Stack.Length = 2, "Next_To_Top modified the length");
      I3 := Int (Stack.Pop);
      begin
         I3 := Int (Stack.Next_To_Top);
         Assert
           (False,
            "Top should have raised Emtpy_Stack when the stack's length is 1");
      exception
         when Stack.Stack_Empty =>
            null;
         when E : others =>
            Assert (False, "Wrong exception raised : " &
                    Ada.Exceptions.Exception_Name (E));
      end;
   end Test_Next_To_Top;

end Stack.Test;
