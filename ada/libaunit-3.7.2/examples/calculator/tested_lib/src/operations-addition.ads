--
--  Copyright (C) 2008, AdaCore
--
with Operands.Ints;
with Operations.Binary;
with Operations.Ints;

package Operations.Addition is new Operations.Binary
  (T => Operands.Ints.Int,
   T_Ret => Operands.Ints.Int,
   The_Operation => Operations.Ints."+");
