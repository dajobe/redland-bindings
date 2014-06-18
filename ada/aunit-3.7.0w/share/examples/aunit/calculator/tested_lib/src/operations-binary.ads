--
--  Copyright (C) 2008, AdaCore
--
with Operands; use Operands;
with Stack;

generic
   type T is new Operands.Operand with private;
   type T_Ret is new Operands.Operand with private;
   with function The_Operation (T1, T2 : T) return T_Ret;
package Operations.Binary is

   type Binary_Operation is new Operation with private;

   procedure Pop (Op : in out Binary_Operation);
   pragma Precondition
     (Stack.Length >= 2
      and then Stack.Top in T'Class
      and then Stack.Next_To_Top in T'Class,
      "precondition for Operations.Binary.Pop");
   pragma Postcondition
     (Stack.Length = Stack.Length'Old - 2,
      "postcondition for Operations.Binary.Pop");
   --  Pops the operands from the stack

   procedure Push (Op : in out Binary_Operation);
   pragma Precondition
     (Stack.Length < Stack.Max_Stack_Size,
      "precondition for Operations.Binary.Push");
   pragma Postcondition
     (Stack.Length = Stack.Length'Old + 1,
      "postcondition for Operations.Binary.Push");
   --  Pushes the operands in the stack

   procedure Execute (Op : in out Binary_Operation);
   --  Execute the operation

private

   type Binary_Operation is new Operation with record
      Op1 : T;
      Op2 : T;
      Res : T_Ret;
   end record;

end Operations.Binary;
