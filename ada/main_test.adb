-- TODO: Do Adjust as in this file

with Ada.Finalization;
with Ada.Text_IO;

procedure Main_Test is

  package A_Pkg is

      type A is new Ada.Finalization.Controlled with null record;

      not overriding procedure Do_It(Object: A);

      overriding procedure Adjust(Object: in out A);

   end A_Pkg;

   package body A_Pkg is

      procedure Do_It(Object: A) is
      begin
         Ada.Text_IO.Put_Line("A");
      end;

      procedure Adjust(Object: in out A) is
      begin
         Do_It(A'Class(Object));
      end;

   end A_Pkg;

  package B_Pkg is

      type B is new A_Pkg.A with null record;

      overriding procedure Do_It(Object: B);

   end B_Pkg;

   package body B_Pkg is

      procedure Do_It(Object: B) is
      begin
         Ada.Text_IO.Put_Line("B");
      end;

   end B_Pkg;

   X: B_Pkg.B;
   Y: B_Pkg.B;

begin
   Y := X;
end Main_Test;
