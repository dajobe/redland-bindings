--
--  Copyright (C) 2008-2010, AdaCore
--

with AUnit.Test_Suites.Tests.Suite;
with AUnit.Test_Cases.Tests.Suite;
with AUnit.Test_Fixtures.Tests.Suite;

package body AUnit_Suite is
   use Test_Suites;

   function Suite return Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite :=
            AUnit.Test_Suites.New_Suite;
   begin
      Add_Test
        (S, AUnit.Test_Suites.Tests.Suite.Test_Suite);
      Add_Test
        (S, AUnit.Test_Cases.Tests.Suite.Test_Suite);
      Add_Test
        (S, AUnit.Test_Fixtures.Tests.Suite.Test_Suite);

      return S;
   end Suite;

end AUnit_Suite;
