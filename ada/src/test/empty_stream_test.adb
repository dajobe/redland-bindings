with AUnit.Assertions; use AUnit.Assertions;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.Stream; use RDF.Redland.Stream;

package body Empty_Stream_Test is

   procedure Test_Empty(T : in out Test_Cases.Test_Case'Class) is
      World: Redland_World_Type;
      Stream: Stream_Type := Empty_Stream(World);
      Counter: Natural := 0;
   begin
      for X in Create_Stream_Iterator(Stream) loop
         Counter := Counter + 1;
      end loop;
      Assert(Counter = 0, "Zero counter");
   end;

   function Name (T : Test_Case)
                  return Test_String is
   begin
      return Format ("Empty Redland stream");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Empty'Access, "Testing empty streams");
   end Register_Tests;

end Empty_Stream_Test;
