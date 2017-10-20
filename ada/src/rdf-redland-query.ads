with Ada.Iterator_Interfaces;
with RDF.Auxiliary.Handled_Record;
with RDF.Raptor.Syntaxes; use RDF.Raptor.Syntaxes;
with RDF.Redland.World; use RDF.Redland.World;

package RDF.Redland.Query is

   package Query_Handled_Record is new RDF.Auxiliary.Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Query_Type_Without_Finalize is new RDF.Redland.Query.Query_Handled_Record.Base_Object with null record;

   subtype Query_Handle is Query_Handled_Record.Access_Type;

   function Get_Query_Language_Description (World: Redland_World_Type_Without_Finalize'Class;
                                            Counter: Natural)
                                            return Raptor_Syntax_Description_Type;

   type Query_Language_Description_Cursor is private;

   function Get_Position (Cursor: Query_Language_Description_Cursor) return Natural;

   function Get_Description (Cursor: Query_Language_Description_Cursor) return Raptor_Syntax_Description_Type;

   function Has_Element (Position: Query_Language_Description_Cursor) return Boolean;

   package Query_Language_Description_Iterators is new Ada.Iterator_Interfaces(Query_Language_Description_Cursor, Has_Element);

   type Query_Language_Description_Iterator is new Query_Language_Description_Iterators.Forward_Iterator with private;

   overriding function First (Object: Query_Language_Description_Iterator) return Query_Language_Description_Cursor;
   overriding function Next (Object: Query_Language_Description_Iterator; Position: Query_Language_Description_Cursor)
                             return Query_Language_Description_Cursor;

   function Create_Query_Language_Descriptions_Iterator (World: Redland_World_Type_Without_Finalize'Class)
                                                         return Query_Language_Description_Iterator;

   -- TODO: Stopped at librdf_new_query()

private

   type Query_Language_Description_Cursor is
      record
         World: Redland_World_Handle;
         Position: Natural;
      end record;

   type Query_Language_Description_Iterator is new Query_Language_Description_Iterators.Forward_Iterator with
      record
         World: Redland_World_Handle;
      end record;

end RDF.Redland.Query;
