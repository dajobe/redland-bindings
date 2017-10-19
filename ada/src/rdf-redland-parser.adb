with Interfaces.C; use Interfaces.C;

package body RDF.Redland.Parser is

   function librdf_parser_check_name (World: Redland_World_Handle; Name: char_array) return int
     with Import, Convention=>C;

   function Parser_Check_Name (World: Redland_World_Type_Without_Finalize'Class; Name: String)
                               return Boolean is
   begin
      return librdf_parser_check_name(Get_Handle(World), To_C(Name)) /= 0;
   end;

   function librdf_parser_get_description (World: Redland_World_Handle; Counter: unsigned)
                                           return Raptor_Syntax_Description_Type
     with Import, Convention=>C;

   function Get_Parser_Description (World: Redland_World_Type_Without_Finalize'Class;
                                    Counter: Natural)
                                    return Raptor_Syntax_Description_Type is
   begin
      return librdf_parser_get_description(Get_Handle(World), unsigned(Counter));
   end;

   function Has_Element (Position: Parser_Description_Cursor) return Boolean is
   begin
      return librdf_parser_get_description(Position.World, unsigned(Position.Position)) /= null;
   end;

   function First (Object: Parser_Description_Iterator) return Parser_Description_Cursor is
   begin
      return (Position=>0, World=>Object.World);
   end;

   function Next (Object: Parser_Description_Iterator; Position: Parser_Description_Cursor)
                  return Parser_Description_Cursor is
   begin
      return (Position=>Position.Position+1, World=>Position.World);
   end;

   function Get_Description (Cursor: Parser_Description_Cursor)
                             return Raptor_Syntax_Description_Type is
   begin
      return librdf_parser_get_description(Cursor.World, unsigned(Cursor.Position));
   end;

   function Get_Position (Cursor: Parser_Description_Cursor) return Natural is
   begin
      return Cursor.Position;
   end;

   function Create_Parser_Descriptions_Iterator(World: Redland_World_Type_Without_Finalize'Class)
                                                return Parser_Description_Iterator is
   begin
      return (World=>Get_Handle(World));
   end;

end RDF.Redland.Parser;
