--
--  Copyright (C) 2008, AdaCore
--
package body Rectangle is

   function Area (Obj : Rectangle_Type) return Natural is
   begin
      return Obj.Width * Obj.Height;
   end Area;

end Rectangle;
