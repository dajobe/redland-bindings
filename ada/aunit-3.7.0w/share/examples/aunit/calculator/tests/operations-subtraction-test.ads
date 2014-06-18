--
--  Copyright (C) 2008, AdaCore
--
with Operations.Binary.Gen_Test;
with Operations.Subtraction;
with Operations.Subtraction_Test_Fixture;
use Operations.Subtraction_Test_Fixture;
package Operations.Subtraction.Test is new Operations.Subtraction.Gen_Test
  (Set_Up);
