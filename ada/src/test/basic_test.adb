with RDF.Raptor;
use all type RDF.Raptor.Flag_Type;

-- This just creates and assigns some variables (and ignores their values)

package body Basic_Test is

   Default_World: RDF.Raptor.World := RDF.Raptor.Open;

   World_With_Some_Flags: RDF.Raptor.World := RDF.Raptor.Open((1=>(Flag=>URI_Interning, Value=>False)));

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case)
                  return Test_String is
   begin
      return Format ("Constructors of RDF worlds");
   end Name;

   -- The test consists solely of creating variables, no test procedures are run.
   procedure Register_Tests (T : in out Test_Case) is
   begin
      null;
   end Register_Tests;

end Basic_Test;
