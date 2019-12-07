--
--  Copyright (C) 2008, AdaCore
--
--  with AUnit.Test_Caller;

with Rectangle.Tests.Suite;
with Square.Tests.Suite;
with Square.Tests.Suite_Liskov;

package body My_Suite is

   Result : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Result.Add_Test (Rectangle.Tests.Suite.Suite);
      Result.Add_Test (Square.Tests.Suite.Suite);
      Result.Add_Test (Square.Tests.Suite_Liskov.Suite);
      return Result'Access;
   end Suite;

end My_Suite;
