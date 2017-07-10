--
--  Copyright (C) 2008, AdaCore
--
package Operations is

   type Operation is abstract tagged null record;

   procedure Pop (Op : in out Operation) is abstract;
   --  Pops the operands from the stack

   procedure Push (Op : in out Operation) is abstract;
   --  Pushes the operands in the stack

   procedure Execute (Op : in out Operation) is abstract;
   --  Execute the operation

end Operations;
