--
--  Copyright (C) 2009-2010, AdaCore
--

with AUnit.Test_Fixtures;
with AUnit.Test_Cases.Tests_Fixtures; use AUnit.Test_Cases.Tests_Fixtures;

package AUnit.Test_Cases.Tests is

   type Fixture is new AUnit.Test_Fixtures.Test_Fixture with record
      TC : aliased The_Test_Case;
   end record;

   procedure Test_Register_Tests (T : in out Fixture);
   procedure Test_Set_Up (T : in out Fixture);
   procedure Test_Torn_Down (T : in out Fixture);
   procedure Test_Run (T : in out Fixture);

end AUnit.Test_Cases.Tests;
