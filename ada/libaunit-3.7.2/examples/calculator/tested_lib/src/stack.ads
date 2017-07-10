--
--  Copyright (C) 2008, AdaCore
--
with Ada.Unchecked_Deallocation;
with Operands; use Operands;

package Stack is

   Stack_Overflow : exception;
   Stack_Empty    : exception;

   Max_Stack_Size : constant Natural := 128;

   procedure Push (Op : Operands.Operand'Class);
   --  Push an operand on the stack
   --  Raises Stack_Overflow if the stack is full

   function Pop return Operands.Operand'Class;
   --  Pop an operand from the stack
   --  Raises Stack_Empty if the stack is empty

   function Length return Natural;
   --  Return the number of objects in the stack

   function Top return Operands.Operand'Class;
   --  Return the operand on the top of the stack without removing it from the
   --  stack.

   function Next_To_Top return Operands.Operand'Class;
   --  Return he operand on the next to top of the stack without removing it
   --  from the stack

private

   type Stack_Index is new Natural range 0 .. Max_Stack_Size;
   Empty_Stack : constant Stack_Index := 0;

   The_Stack : array (Stack_Index range 1 .. Stack_Index'Last)
                 of Operand_Access;
   The_Stack_Index : Stack_Index := Empty_Stack;

   procedure Free is new Ada.Unchecked_Deallocation
     (Operands.Operand'Class, Operand_Access);

end Stack;
