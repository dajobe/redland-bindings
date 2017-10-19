with Ada.Iterator_Interfaces;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.Syntaxes; use RDF.Raptor.Syntaxes;
with RDF.Redland.World; use RDF.Redland.World;

package RDF.Redland.Parser is

   package Parser_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Parser_Type_Without_Finalize is new Parser_Handled_Record.Base_Object with null record;

   subtype Parser_Handle is Parser_Handled_Record.Access_Type;

   function Parser_Check_Name (World: Redland_World_Type_Without_Finalize'Class; Name: String)
                               return Boolean;

   function Get_Parser_Description (World: Redland_World_Type_Without_Finalize'Class;
                                    Counter: Natural)
                                    return Raptor_Syntax_Description_Type;

   type Parser_Description_Cursor   is private;

   function Get_Position (Cursor: Parser_Description_Cursor) return Natural;

   function Get_Description (Cursor: Parser_Description_Cursor) return Raptor_Syntax_Description_Type;

   function Has_Element (Position: Parser_Description_Cursor) return Boolean;

   package Parser_Description_Iterators is new Ada.Iterator_Interfaces(Parser_Description_Cursor, Has_Element);

   type Parser_Description_Iterator is new Parser_Description_Iterators.Forward_Iterator with private;

   overriding function First (Object: Parser_Description_Iterator) return Parser_Description_Cursor;
   overriding function Next (Object: Parser_Description_Iterator; Position: Parser_Description_Cursor)
                             return Parser_Description_Cursor;

   function Create_Parser_Descriptions_Iterator (World: Redland_World_Type_Without_Finalize'Class)
                                                 return Parser_Description_Iterator;

   -- Stopped at librdf_parser_guess_name()

private

   type Parser_Description_Cursor is
      record
         World: Redland_World_Handle;
         Position: Natural;
      end record;

   type Parser_Description_Iterator is new Parser_Description_Iterators.Forward_Iterator with
      record
         World: Redland_World_Handle;
      end record;

end RDF.Redland.Parser;
