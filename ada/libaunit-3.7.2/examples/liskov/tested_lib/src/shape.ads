--
--  Copyright (C) 2008, AdaCore
--

package Shape is

   type Shape_Type is abstract tagged private;
   type Shape_Access is access all Shape_Type'Class;

   --  Additional functional API for expressing pre/postconditions
   --  & invariants:
   --    function New_Shape (W, H : Natural) return Shape;
   --    function Set_Width (S : Shape; W : Natural) return Shape;
   --    function Set_Height (S : Shape; H : Natural) return Shape;
   --
   --  Class invariants:
   --  for_all W, H in Natural:
   --     Set_Width  (New_Shape (W, H), X) = New_Shape (X, H))
   --     Set_Height (New_Shape (W, H), X) = New_Shape (W, X))

   function Width (Obj : Shape_Type) return Natural;
   function Height (Obj : Shape_Type) return Natural;

   procedure Set_Width (Obj : in out Shape_Type; W : Natural);
--     pragma Postcondition
--       (Width (Obj) = W                     -- expected result
--        and Height (Obj) = Height (Obj'Old) -- independence
--       );

   procedure Set_Height (Obj : in out Shape_Type; H : Natural);
--     pragma Postcondition
--       (Height (Obj) = H                     -- expected result
--        and Width (Obj) = Width (Obj'Old)    -- independence
--       );

   function Area (Obj : Shape_Type) return Natural is abstract;

private
   type Shape_Type is abstract tagged record
      Width : Natural;
      Height : Natural;
   end record;
end Shape;
