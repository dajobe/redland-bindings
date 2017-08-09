with Ada.Iterator_Interfaces;
--with Interfaces.C; use Interfaces.C;
--with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Rasqal.World; use RDF.Rasqal.World;
--with RDF.Rasqal.URI; use RDF.Rasqal.URI;
with RDF.Raptor.Syntaxes;

package RDF.Rasqal.Syntaxes is

   -- TODO: Add low-level interface (without iterators)

   subtype Syntax_Description_Type is RDF.Raptor.Syntaxes.Syntax_Description_Type;

   type Query_Language_Description_Cursor is private;
   type Query_Results_Format_Description_Cursor is private;

   function Get_Position (Cursor: Query_Language_Description_Cursor    ) return Natural;
   function Get_Position (Cursor: Query_Results_Format_Description_Cursor) return Natural;

   function Get_Description (Cursor: Query_Language_Description_Cursor    ) return Syntax_Description_Type;
   function Get_Description (Cursor: Query_Results_Format_Description_Cursor) return Syntax_Description_Type;

   function Has_Element (Position: Query_Language_Description_Cursor    ) return Boolean;
   function Has_Element (Position: Query_Results_Format_Description_Cursor) return Boolean;

   package Query_Language_Description_Iterators     is new Ada.Iterator_Interfaces(Query_Language_Description_Cursor    , Has_Element);
   package Query_Results_Format_Description_Iterators is new Ada.Iterator_Interfaces(Query_Results_Format_Description_Cursor, Has_Element);

   type Query_Language_Description_Iterator     is new Query_Language_Description_Iterators    .Forward_Iterator with private;
   type Query_Results_Format_Description_Iterator is new Query_Results_Format_Description_Iterators.Forward_Iterator with private;

   overriding function First (Object: Query_Language_Description_Iterator) return Query_Language_Description_Cursor;
   overriding function Next (Object: Query_Language_Description_Iterator; Position: Query_Language_Description_Cursor) return Query_Language_Description_Cursor;

   overriding function First (Object: Query_Results_Format_Description_Iterator) return Query_Results_Format_Description_Cursor;
   overriding function Next (Object: Query_Results_Format_Description_Iterator; Position: Query_Results_Format_Description_Cursor) return Query_Results_Format_Description_Cursor;

   not overriding function Create_Query_Language_Descriptions_Iterator     (World: RDF.Rasqal.World.World_Type_Without_Finalize'Class) return Query_Language_Description_Iterator;
   not overriding function Create_Query_Results_Format_Descriptions_Iterator (World: RDF.Rasqal.World.World_Type_Without_Finalize'Class) return Query_Results_Format_Description_Iterator;

   function Language_Name_Check (World: RDF.Rasqal.World.World_Type_Without_Finalize'Class; Name: String) return Boolean;

private

   type Query_Language_Description_Cursor is
      record
         World: RDF.Rasqal.World.Handle_Type;
         Position: Natural;
      end record;

   type Query_Results_Format_Description_Cursor is
      record
         World: RDF.Rasqal.World.Handle_Type;
         Position: Natural;
      end record;

   type Query_Language_Description_Iterator is new Query_Language_Description_Iterators.Forward_Iterator with
      record
         World: RDF.Rasqal.World.Handle_Type;
      end record;

   type Query_Results_Format_Description_Iterator is new Query_Results_Format_Description_Iterators.Forward_Iterator with
      record
         World: RDF.Rasqal.World.Handle_Type;
      end record;

end RDF.Rasqal.Syntaxes;
