with Ada.Iterator_Interfaces;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.Syntaxes; use RDF.Raptor.Syntaxes;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.URI; use RDF.Redland.URI;

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

   -- I do not bind librdf_parser_guess_name() and librdf_parser_guess_name2()
   -- because of http://bugs.librdf.org/mantis/view.php?id=637

   type Parser_Type is new Parser_Type_Without_Finalize with null record;

   not overriding function Create (World: Redland_World_Type_Without_Finalize'Class;
                                   Name, Mime_Type: String;
                                   Type_URI: URI_Type_Without_Finalize'Class)
                                   return Parser_Type;

   -- Stopped at librdf_free_parser()

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
