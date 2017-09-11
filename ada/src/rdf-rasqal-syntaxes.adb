with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
--  with Interfaces.C.Pointers;
--  with RDF.Rasqal.World; use RDF.Rasqal.World;

package body RDF.Rasqal.Syntaxes is

   type Syntax_Description_Access is access all Syntax_Description_Type
      with Convention=>C;

   function rasqal_world_get_query_language_description (World: RDF.Rasqal.World.Handle_Type; Counter: unsigned)
                                                         return Syntax_Description_Access
      with Import, Convention=>C;

   function rasqal_world_get_query_results_format_description (World: RDF.Rasqal.World.Handle_Type; Counter: unsigned)
                                                               return Syntax_Description_Access
      with Import, Convention=>C;

   function Get_Query_Language_Description (World: World_Type; Counter: Unsigned) return Syntax_Description_Type is
   begin
      return rasqal_world_get_query_language_description(Get_Handle(World), Counter).all;
   end;

   function Get_Query_Results_Format_Description (World: World_Type; Counter: Unsigned) return Syntax_Description_Type is
   begin
      return rasqal_world_get_query_results_format_description(Get_Handle(World), Counter).all;
   end;

   --package String_Ptrs is new Interfaces.C.Pointers(size_t, chars_ptr, chars_ptr_array, Null_Ptr);

   --use String_Ptrs;

   function Get_Position (Cursor: Query_Language_Description_Cursor      ) return Natural is (Cursor.Position);
   function Get_Position (Cursor: Query_Results_Format_Description_Cursor) return Natural is (Cursor.Position);

   function Get_Description (Cursor: Query_Language_Description_Cursor    ) return Syntax_Description_Type is
   begin
      return Get_Query_Language_Description (From_Handle(Cursor.World), unsigned(Cursor.Position));
   end;

   function Get_Description (Cursor: Query_Results_Format_Description_Cursor) return Syntax_Description_Type is
   begin
      return Get_Query_Results_Format_Description (From_Handle(Cursor.World), unsigned(Cursor.Position));
   end;

   function Has_Element (Position: Query_Language_Description_Cursor) return Boolean is
   begin
      return rasqal_world_get_query_language_description(Position.World, unsigned(Position.Position)) /= null;
   end;

   function Has_Element (Position: Query_Results_Format_Description_Cursor) return Boolean is
   begin
      return rasqal_world_get_query_results_format_description(Position.World, unsigned(Position.Position)) /= null;
   end;

   function First (Object: Query_Language_Description_Iterator) return Query_Language_Description_Cursor is
   begin
      return (Position=>0, World=>Object.World);
   end;

   function Next (Object: Query_Language_Description_Iterator; Position: Query_Language_Description_Cursor) return Query_Language_Description_Cursor is
   begin
      return (Position=>Position.Position+1, World=>Position.World);
   end;

   function First (Object: Query_Results_Format_Description_Iterator) return Query_Results_Format_Description_Cursor is
   begin
      return (Position=>0, World=>Object.World);
   end;

   function Next (Object: Query_Results_Format_Description_Iterator; Position: Query_Results_Format_Description_Cursor) return Query_Results_Format_Description_Cursor is
   begin
      return (Position=>Position.Position+1, World=>Position.World);
   end;

   function Create_Query_Language_Descriptions_Iterator(World: RDF.Rasqal.World.World_Type_Without_Finalize'Class) return Query_Language_Description_Iterator is
   begin
      return (World=>Get_Handle(World));
   end;

   function Create_Query_Results_Format_Descriptions_Iterator (World: RDF.Rasqal.World.World_Type_Without_Finalize'Class) return Query_Results_Format_Description_Iterator is
   begin
      return (World=>Get_Handle(World));
   end;

   function rasqal_language_name_check (World: RDF.Rasqal.World.Handle_Type; Name: char_array) return int
     with Import, Convention=>C;

   function Language_Name_Check (World: RDF.Rasqal.World.World_Type_Without_Finalize'Class; Name: String) return Boolean is
   begin
      return rasqal_language_name_check(Get_Handle(World), To_C(Name)) /= 0;
   end;

end RDF.Rasqal.Syntaxes;
