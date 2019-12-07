--
--  Copyright (C) 2008, AdaCore
--
package body Stack is

   ----------
   -- Push --
   ----------

   procedure Push (Op : Operands.Operand'Class) is
   begin
      if The_Stack_Index = Stack_Index'Last then
         raise Stack_Overflow;
      end if;

      The_Stack_Index := The_Stack_Index + 1;
      The_Stack (The_Stack_Index) := new Operands.Operand'Class'(Op);
   end Push;

   ---------
   -- Pop --
   ---------

   function Pop return Operands.Operand'Class is
   begin
      if The_Stack_Index = Empty_Stack then
         raise Stack_Empty;
      end if;

      declare
         Op : constant Operands.Operand'Class :=
                The_Stack (The_Stack_Index).all;
      begin
         Free (The_Stack (The_Stack_Index));
         The_Stack_Index := The_Stack_Index - 1;
         return Op;
      end;
   end Pop;

   ------------
   -- Length --
   ------------

   function Length return Natural is
   begin
      return Natural (The_Stack_Index);
   end Length;

   --------------
   -- Top_Type --
   --------------

   function Top return Operands.Operand'Class is
   begin
      if The_Stack_Index = 0 then
         raise Stack_Empty;
      end if;

      return The_Stack (The_Stack_Index).all;
   end Top;

   ----------------------
   -- Next_To_Top_Type --
   ----------------------

   function Next_To_Top return Operands.Operand'Class is
   begin
      if The_Stack_Index < 2 then
         raise Stack_Empty;
      end if;

      return The_Stack (The_Stack_Index - 1).all;
   end Next_To_Top;

end Stack;
