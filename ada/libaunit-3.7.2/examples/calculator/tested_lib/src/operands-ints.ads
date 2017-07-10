--
--  Copyright (C) 2008, AdaCore
--
package Operands.Ints is

   type Int is new Operand with private;

   function Image (Opnd : Int) return String;

   function Value (Opnd : Int) return Integer;

   procedure Set (Opnd : in out Int; Value : Integer);

private

   type Int is new Operand with record
      Value : Integer;
   end record;

end Operands.Ints;
