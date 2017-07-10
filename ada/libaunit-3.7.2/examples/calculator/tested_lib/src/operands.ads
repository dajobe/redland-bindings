--
--  Copyright (C) 2008, AdaCore
--
package Operands is

   type Operand is abstract tagged null record;
   type Operand_Access is access all Operand'Class;

   function Image (Opnd : Operand) return String is abstract;

end Operands;
