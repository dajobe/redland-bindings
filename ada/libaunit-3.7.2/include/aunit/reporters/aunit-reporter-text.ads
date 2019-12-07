------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   A U N I T . R E P O R T E R . T E X T                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                      Copyright (C) 2000-2013, AdaCore                    --
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

--  Very simple reporter to console
package AUnit.Reporter.Text is

   type Text_Reporter is new Reporter with private;

   procedure Set_Use_ANSI_Colors
     (Engine : in out Text_Reporter;
      Value  : Boolean);
   --  Setting this value will enable colors output on an ANSI compatible
   --  terminal.
   --  By default, no color is used.

   procedure Report (Engine  : Text_Reporter;
                     R       : in out Result'Class;
                     Options : AUnit_Options := Default_Options);

   procedure Report_OK_Tests (Engine : Text_Reporter;
                              R      : in out Result'Class);
   procedure Report_Fail_Tests (Engine : Text_Reporter;
                                R      : in out Result'Class);
   procedure Report_Error_Tests (Engine : Text_Reporter;
                                 R      : in out Result'Class);
   --  These subprograms implement the various parts of the Report. You
   --  can therefore chose in which order to report the various categories,
   --  and whether or not to report them.
   --  After calling any of these, the list of results has been modified in
   --  R, so you should get the counts first.

private

   type Text_Reporter is new Reporter with record
      Use_ANSI : Boolean := False;
   end record;

end AUnit.Reporter.Text;
