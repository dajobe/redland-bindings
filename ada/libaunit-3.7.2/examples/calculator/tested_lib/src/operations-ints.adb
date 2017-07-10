--
--  Copyright (C) 2008, AdaCore
--
package body Operations.Ints is

   function "+" (Op1, Op2 : Int) return Int is
      Ret : Int;
   begin
      Ret.Set (Op1.Value + Op2.Value);
      return Ret;
   end "+";

   function "-" (Op1, Op2 : Int) return Int is
      Ret : Int;
   begin
      Ret.Set (Op1.Value - Op2.Value);
      return Ret;
   end "-";

end Operations.Ints;
