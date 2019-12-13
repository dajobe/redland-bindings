--
--  Copyright (C) 2008, AdaCore
--
package body Math is

   function "+" (I1, I2 : Int) return Int is
   begin
      return Int (Integer (I1) + Integer (I2));
   end "+";

   function "-" (I1, I2 : Int) return Int is
   begin
      return Int (Integer (I1) - Integer (I2));
   end "-";

end Math;
