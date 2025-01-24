--
--  Copyright (C) 2008, AdaCore
--
with AUnit.Test_Fixtures;

package Stack.Test is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Tear_Down (T : in out Test);

   procedure Test_Push (T : in out Test);
   --  Test for Stack.Push

   procedure Test_Pop (T : in out Test);
   --  Test for Stack.Pop

   procedure Test_Length (T : in out Test);
   --  Test for Stack.Length

   procedure Test_Top (T : in out Test);
   --  Test for Stack.Top

   procedure Test_Next_To_Top (T : in out Test);
   --  Test for Stack.Next_To_Top

end Stack.Test;
