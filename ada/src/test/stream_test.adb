with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AUnit.Assertions; use AUnit.Assertions;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.URI; use RDF.Redland.URI;
with RDF.Redland.Stream; use RDF.Redland.Stream;
with RDF.Redland.Storage; use RDF.Redland.Storage;
with RDF.Redland.Model; use RDF.Redland.Model;
with AUnit.Test_Cases;
with Our_Test; use Our_Test;

package body Stream_Test is

   procedure Test_Empty(T : in out Test_Cases.Test_Case'Class) is
      World: Redland_World_Type;
      Storage: Storage_Type := Create(World, "memory", "test");
      Model: Model_Type := Create(World, Storage);
      T2: Our_Test_Case renames Our_Test_Case(T);
      Directory: constant String := To_String(Get_Environment(T2).Directory);
      RDF_File: constant String := Directory & "/../data/dc.nt";
   begin
      Load(Model, From_Filename(World, RDF_File));
      declare
         Stream: Stream_Type := As_Stream(Model);
         Counter: Natural := 0;
      begin
         for X in Create_Stream_Iterator(Stream) loop
            Counter := Counter + 1;
         end loop;
         Assert(Counter = 3, "Counter = 3");
      end;
   end;

   function Name (T : Test_Case)
                  return Test_String is
   begin
      return Format ("Redland stream");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Empty'Access, "Testing stream elements count");
   end Register_Tests;

end Stream_Test;
