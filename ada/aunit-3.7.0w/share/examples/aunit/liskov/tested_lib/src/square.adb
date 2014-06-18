--
--  Copyright (C) 2008, AdaCore
--
with Rectangle; use Rectangle;
package body Square is

   procedure Set_Width (Obj : in out Square_Type; W : Natural) is
   begin
      Rectangle_Type (Obj).Set_Width (W);
      Rectangle_Type (Obj).Set_Height (W);
   end Set_Width;

   procedure Set_Height (Obj : in out Square_Type; H : Natural)
      renames Set_Width;

end Square;
