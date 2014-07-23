with AUnit.Test_Cases;
with RDF.Raptor.World;
use all type RDF.Raptor.World.Flag_Type;

package body Basic_Test is

   -- This just creates and assigns some variables (and ignores their values)
   procedure Test_Open(T : in out Test_Cases.Test_Case'Class) is
      Default_World: RDF.Raptor.World.World_Type := RDF.Raptor.World.Open;
      World_With_Some_Flags: RDF.Raptor.World.World_Type := RDF.Raptor.World.Open((1=>(Flag=>URI_Interning, Value=>False)));
   begin
      null;
   end;

   function Name (T : Test_Case)
                  return Test_String is
   begin
      return Format ("Constructors of RDF worlds");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Open'Access, "Testing the constructors and function Open");
   end Register_Tests;

end Basic_Test;
