with AUnit.Assertions;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Rasqal.World; use RDF.Rasqal.World;
with RDF.Raptor.Syntaxes; use RDF.Raptor.Syntaxes;
with RDF.Rasqal.Syntaxes; use RDF.Rasqal.Syntaxes;

package body Syntaxes_Test is

   procedure Test_Enum_Syntaxes (T : in out Test_Cases.Test_Case'Class) is
      World2: Rasqal_World_Type := Open;
      World: Raptor_World_Type_Without_Finalize := Get_Raptor(World2);

      Iterator: Parser_Description_Iterator := Create_Parser_Descriptions_Iterator(World);
      Cursor: Parser_Description_Cursor := First(Iterator);
      Iterator2: Serializer_Description_Iterator := Create_Serializer_Descriptions_Iterator(World);
      Cursor2: Serializer_Description_Cursor := First(Iterator2);
      Iterator3: Query_Language_Description_Iterator := Create_Query_Language_Descriptions_Iterator(World2);
      Cursor3: Query_Language_Description_Cursor := First(Iterator3);
      Iterator4: Query_Results_Format_Description_Iterator := Create_Query_Results_Format_Descriptions_Iterator(World2);
      Cursor4: Query_Results_Format_Description_Cursor := First(Iterator4);
   begin
      -- Does not compile with GCC 4.9.1
      while Has_Element(Cursor) loop
         Cursor := Next(Iterator, Cursor);
      end loop;
      while Has_Element(Cursor2) loop
         Cursor2 := Next(Iterator2, Cursor2);
      end loop;
      while Has_Element(Cursor3) loop
         Cursor3 := Next(Iterator3, Cursor3);
      end loop;
      while Has_Element(Cursor4) loop
         Cursor4 := Next(Iterator4, Cursor4);
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
