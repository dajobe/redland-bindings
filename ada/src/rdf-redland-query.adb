with Interfaces.C; use Interfaces.C;

package body RDF.Redland.Query is

   function librdf_query_language_get_description (World: Redland_World_Handle; Counter: unsigned)
                                                   return Raptor_Syntax_Description_Type
     with Import, Convention=>C;

   function Get_Query_Language_Description (World: Redland_World_Type_Without_Finalize'Class;
                                            Counter: Natural)
                                            return Raptor_Syntax_Description_Type is
   begin
      return librdf_query_language_get_description(Get_Handle(World), unsigned(Counter));
   end;

   function Has_Element (Position: Query_Language_Description_Cursor) return Boolean is
   begin
      return librdf_query_language_get_description(Position.World, unsigned(Position.Position)) /= null;
   end;

   function First (Object: Query_Language_Description_Iterator) return Query_Language_Description_Cursor is
   begin
      return (Position=>0, World=>Object.World);
   end;

   function Next (Object: Query_Language_Description_Iterator; Position: Query_Language_Description_Cursor)
                  return Query_Language_Description_Cursor is
   begin
      return (Position=>Position.Position+1, World=>Position.World);
   end;

   function Get_Description (Cursor: Query_Language_Description_Cursor)
                             return Raptor_Syntax_Description_Type is
   begin
      return librdf_query_language_get_description(Cursor.World, unsigned(Cursor.Position));
   end;

   function Get_Position (Cursor: Query_Language_Description_Cursor) return Natural is
   begin
      return Cursor.Position;
   end;

   function Create_Query_Language_Descriptions_Iterator(World: Redland_World_Type_Without_Finalize'Class)
                                                        return Query_Language_Description_Iterator is
   begin
      return (World=>Get_Handle(World));
   end;

end RDF.Redland.Query;
