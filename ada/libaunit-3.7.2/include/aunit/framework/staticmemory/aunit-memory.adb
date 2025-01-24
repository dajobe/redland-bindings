------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         A U N I T . M E M O R Y                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2003 Free Software Foundation, Inc.          --
--                     Copyright (C) 2008-2011, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT is maintained by AdaCore (http://www.adacore.com)                   --
--                                                                          --
------------------------------------------------------------------------------

--  Dummy implementation.

with System.Storage_Elements;
with Unchecked_Conversion;

package body AUnit.Memory is

   package SSE renames System.Storage_Elements;

   Default_Size : constant := 100 * 1_024;

   type Mark_Id is new SSE.Integer_Address;

   type Memory is array (Mark_Id range <>) of SSE.Storage_Element;
   for Memory'Alignment use Standard'Maximum_Alignment;

   Mem : Memory (1 .. Default_Size);

   Top : Mark_Id := Mem'First;

   function To_Mark_Id is new Unchecked_Conversion
     (size_t, Mark_Id);

   -----------
   -- Alloc --
   -----------

   function AUnit_Alloc (Size : size_t) return System.Address is
      Max_Align    : constant Mark_Id := Mark_Id (Standard'Maximum_Alignment);
      Max_Size     : Mark_Id :=
                       ((To_Mark_Id (Size) + Max_Align - 1) / Max_Align)
                       * Max_Align;
      Location     : constant Mark_Id := Top;
   begin
      if Max_Size = 0 then
         Max_Size := Max_Align;
      end if;

      if Size = size_t'Last then
         raise Storage_Error;
      end if;

      Top := Top + Max_Size;

      if Top > Default_Size then
         raise Storage_Error;
      end if;

      return Mem (Location)'Address;
   end AUnit_Alloc;

   ----------
   -- Free --
   ----------

   procedure AUnit_Free (Obj : System.Address) is
      pragma Unreferenced (Obj);
   begin
      null;
   end AUnit_Free;

end AUnit.Memory;
