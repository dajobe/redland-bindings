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

   function librdf_new_query (World: Redland_World_Handle;
                              Name: char_array;
                              URI: URI_Handle;
                              Query_String: char_array;
                              Base_URI: URI_Handle)
                              return Query_Handle
     with Import, Convention=>C;

   function Create (World: Redland_World_Type_Without_Finalize'Class;
                    Name: String;
                    Query_String: String;
                    URI, Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                    return Query_Type is
      Handle: constant Query_Handle := librdf_new_query(Get_Handle(World),
                                                        To_C(Name),
                                                        Get_Handle(URI),
                                                        To_C(Query_String),
                                                        Get_Handle(Base_URI));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_new_query_from_query (Query: Query_Handle) return Query_Handle
     with Import, Convention=>C;

   function Adjust_Handle (Object: Query_Type_Without_Finalize; Handle: Query_Handle) return Query_Handle is
   begin
      return librdf_new_query_from_query(Handle);
   end;

   procedure librdf_free_query (Query: Query_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Query_Type_Without_Finalize; Handle: Query_Handle) is
   begin
      librdf_free_query(Handle);
   end;

end RDF.Redland.Query;
