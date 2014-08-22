with AUnit.Test_Cases;
with AUnit.Assertions;
with RDF.Raptor.World;
with RDF.Raptor.Syntaxes; use RDF.Raptor.Syntaxes;

package body Syntaxes_Test is

   procedure Test_Enum_Syntaxes (T : in out Test_Cases.Test_Case'Class) is
      World: RDF.Raptor.World.World_Type;

      Iterator: Parser_Description_Iterator := Create_Parser_Descriptions_Iterator(World);
      Cursor: Parser_Description_Cursor := First(Iterator);
      Iterator2: Serializer_Description_Iterator := Create_Serializer_Descriptions_Iterator(World);
      Cursor2: Serializer_Description_Cursor := First(Iterator2);
   begin
      -- Does not compile with GCC 4.9.1
      while Has_Element(Cursor) loop
         Cursor := Next(Iterator, Cursor);
      end loop;
      while Has_Element(Cursor2) loop
         Cursor2 := Next(Iterator2, Cursor2);
      end loop;
   end;

   function Name (T : Test_Case)
                  return Test_String is
   begin
      return Format ("Enumerating syntaxes");
   end Name;

   procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Enum_Syntaxes'Access, "Enumerating syntaxes is finite");
   end Register_Tests;

end Syntaxes_Test;
