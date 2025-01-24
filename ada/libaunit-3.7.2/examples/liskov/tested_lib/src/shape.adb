--
--  Copyright (C) 2008, AdaCore
--
package body Shape is

   procedure Set_Width (Obj : in out Shape_Type; W : Natural) is
   begin
      Obj.Width := W;
   end Set_Width;

   procedure Set_Height (Obj : in out Shape_Type; H : Natural) is
   begin
      Obj.Height := H;
   end Set_Height;

   function Width (Obj : in Shape_Type) return Natural is
   begin
      return Obj.Width;
   end Width;

   function Height (Obj : in Shape_Type) return Natural is
   begin
      return Obj.Height;
   end Height;

end Shape;
