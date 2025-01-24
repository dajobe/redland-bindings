--
--  Copyright (C) 2008, AdaCore
--
package body Operands.Ints is

   function Image (Opnd : Int) return String is
   begin
      return Integer'Image (Opnd.Value);
   end Image;

   function Value (Opnd : Int) return Integer is
   begin
      return Opnd.Value;
   end Value;

   procedure Set (Opnd : in out Int; Value : Integer) is
   begin
      Opnd.Value := Value;
   end Set;

end Operands.Ints;
