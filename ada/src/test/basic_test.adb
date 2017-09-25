with AUnit.Test_Cases;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Rasqal.World; use RDF.Rasqal.World;
--  use all type Raptor_Flag_Type;

package body Basic_Test is

   -- This just creates and assigns some variables (and ignores their values)
   procedure Test_Open(T : in out Test_Cases.Test_Case'Class) is
      Default_World: Raptor_World_Type := Open;
      World_With_Some_Flags: Raptor_World_Type := Open((1=>(Flag=>URI_Interning, Value=>False)));

      World2: Rasqal_World_Type := Open;
      World: Raptor_World_Type_Without_Finalize := Get_Raptor(World2);
   begin
      null;
   end;

   function Name (T : Test_Case)
                  return Test_String is
   begin
      return Format ("Constructors of worlds");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Open'Access, "Testing the constructors and function Open");
   end Register_Tests;

end Basic_Test;
