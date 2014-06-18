--
--  Copyright (C) 2008, AdaCore
--
with Operations.Binary.Gen_Test;
with Operations.Addition;
with Operations.Addition_Test_Fixture; use Operations.Addition_Test_Fixture;
package Operations.Addition.Test is new Operations.Addition.Gen_Test
  (Set_Up);
