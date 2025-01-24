with Ada.Strings.Unbounded;
with AUnit.Environments;

package Raptor_Environments is

   type Raptor_Environment is new AUnit.Environments.Environment with
      record
         Directory: Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Raptor_Environment_Access is access all Raptor_Environment;

end Raptor_Environments;
