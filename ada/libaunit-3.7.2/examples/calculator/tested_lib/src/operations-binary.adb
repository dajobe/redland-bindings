--
--  Copyright (C) 2008, AdaCore
--
package body Operations.Binary is

   ---------
   -- Pop --
   ---------

   procedure Pop (Op : in out Binary_Operation) is
   begin
      Op.Op2 := T (Stack.Pop);
      Op.Op1 := T (Stack.Pop);
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push (Op : in out Binary_Operation) is
   begin
      Stack.Push (Op.Res);
   end Push;

   -------------
   -- Execute --
   -------------

   procedure Execute (Op : in out Binary_Operation) is
   begin
      Op.Res := The_Operation (Op.Op1, Op.Op2);
   end Execute;

end Operations.Binary;
