with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

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

   function librdf_new_parser (World: Redland_World_Handle;
                               Name: char_array;
                               Mime_Type: chars_ptr;
                               Type_URI: URI_Handle)
                               return Parser_Handle
     with Import, Convention=>C;

   function Create (World: Redland_World_Type_Without_Finalize'Class;
                    Name, Mime_Type: String := "";
                    Type_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                    return Parser_Type is
      Mime_Type2: aliased char_array := To_C(Mime_Type);
      Handle: constant Parser_Handle :=
        librdf_new_parser(Get_Handle(World),
                          To_C(Name),
                          (if Mime_Type = "" then Null_Ptr else To_Chars_Ptr(Mime_Type2'Unchecked_Access)),
                           Get_Handle(Type_URI));
   begin
      return From_Handle(Handle);
   end;

   procedure librdf_free_parser (Handle: Parser_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Parser_Type; Handle: Parser_Handle) is
   begin
      librdf_free_parser(Handle);
   end;

   function librdf_parser_parse_as_stream (Parser: Parser_Handle; URI, Base_URI: URI_Handle)
                                           return Stream_Handle
     with Import, Convention=>C;

   function As_Stream (Parser: Parser_Type_Without_Finalize;
                       URI: URI_Type_Without_Finalize'Class;
                       Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                       return Stream_Type is
      Handle: constant Stream_Handle :=
        librdf_parser_parse_as_stream(Get_Handle(Parser), Get_Handle(URI), Get_Handle(Base_URI));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_parser_parse_into_model (Parser: Parser_Handle;
                                            URI, Base_URI: URI_Handle;
                                            Model: Model_Handle)
                                            return int
     with Import, Convention=>C;

   procedure Parse_Into_Model (Parser: Parser_Type_Without_Finalize;
                               Model: in out Model_Type_Without_Finalize'Class;
                               URI: URI_Type_Without_Finalize'Class;
                               Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null))) is
   begin
      if librdf_parser_parse_into_model(Get_Handle(Parser),
                                        Get_Handle(URI),
                                        Get_Handle(Base_URI),
                                        Get_Handle(Model)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_parser_parse_file_handle_as_stream (Parser: Parser_Handle;
                                                       File: RDF.Auxiliary.C_File_Access;
                                                       Close: int;
                                                       Base_URI: URI_Handle)
                                                       return Stream_Handle
     with Import, Convention=>C;

   function Parse_File_Handle_As_Stream (Parser: Parser_Type_Without_Finalize;
                                         File: RDF.Auxiliary.C_File_Access;
                                         Close: Boolean;
                                         Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                                         return Stream_Type is
      Handle: constant Stream_Handle :=
        librdf_parser_parse_file_handle_as_stream(Get_Handle(Parser),
                                                  File,
                                                  (if Close then 1 else 0),
                                                  Get_Handle(Base_URI));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_parser_parse_file_handle_into_model (Parser: Parser_Handle;
                                                        File: RDF.Auxiliary.C_File_Access;
                                                        Close: int;
                                                        Base_URI: URI_Handle;
                                                        Model: Model_Handle)
                                                        return int
     with Import, Convention=>C;

   procedure Parse_File_Handle_Into_Model (Parser: Parser_Type_Without_Finalize;
                                           File: RDF.Auxiliary.C_File_Access;
                                           Close: Boolean;
                                           Model: in out Model_Type_Without_Finalize'Class;
                                           Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null))) is
   begin
      if librdf_parser_parse_file_handle_into_model(Get_Handle(Parser),
                                                    File,
                                                    (if Close then 1 else 0),
                                                    Get_Handle(Base_URI),
                                                    Get_Handle(model)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

end RDF.Redland.Parser;
