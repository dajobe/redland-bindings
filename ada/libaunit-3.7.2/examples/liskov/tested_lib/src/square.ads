--
--  Copyright (C) 2008, AdaCore
--

with Rectangle;

package Square is

   type Square_Type is new Rectangle.Rectangle_Type with private;
   --  class invariant
   --   for all Obj : Eight (Obj) = Width (Obj)

   procedure Set_Width (Obj : in out Square_Type; W : Natural);
--     pragma Postcondition
--       (Height (Obj) = Width (Obj)  -- this is the class invariant
--       );

   procedure Set_Height (Obj : in out Square_Type; H : Natural);
--     pragma Postcondition
--       (Height (Obj) = Width (Obj)  -- this is the class invariant
--       );

private

   type Square_Type is new Rectangle.Rectangle_Type with null record;

end Square;
