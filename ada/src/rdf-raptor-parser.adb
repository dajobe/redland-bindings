with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary; use RDF.Auxiliary;
with RDF.Auxiliary.C_String_Holders; use RDF.Auxiliary.C_String_Holders;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.Namespaces; use RDF.Raptor.Namespaces;
with RDF.Raptor.Statement; use RDF.Raptor.Statement;
with RDF.Raptor.Log; use RDF.Raptor.Log;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.IOStream;

package body RDF.Raptor.Parser is

--     use all type Parser_Type;

   function C_Raptor_New_Parser (World: RDF.Raptor.World.Handle_Type; Name: char_array) return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_parser";

   function Create (World: RDF.Raptor.World.World_Type_Without_Finalize'Class; Name: String) return Parser_Type is
   begin
      return From_Non_Null_Handle( C_Raptor_New_Parser(Get_Handle(World), To_C(Name, Append_Nul=>True)) );
   end;

   function C_Raptor_New_Parser_For_Content (World: RDF.Raptor.World.Handle_Type;
                                             URI: RDF.Raptor.URI.Handle_Type;
                                             MIME_Type: chars_ptr;
                                             Buffer: chars_ptr;
                                             Len: size_t;
                                             Identifier: chars_ptr) return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_parser_for_content";

   function Create_From_Content (World: RDF.Raptor.World.World_Type_Without_Finalize'Class;
                                 URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class;
                                 Mime_Type : String_Holders.Holder;
                                 Buffer    : String_Holders.Holder;
                                 Identifier: String_Holders.Holder)
                                 return Parser_Type is
      Mime_Type_N : C_String_Holder := To_C_String_Holder(Mime_Type);
      Buffer_N    : C_String_Holder := To_C_String_Holder(Buffer);
      Identifier_N: C_String_Holder := To_C_String_Holder(Identifier);
   begin
      return  From_Non_Null_Handle( C_Raptor_New_Parser_For_Content(Get_Handle(World),
                                                                    Get_Handle(URI),
                                                                    C_String(Mime_Type_N),
                                                                    C_String(Buffer_N),
                                                                    size_t(Length(Buffer)),
                                                                    C_String(Identifier_N)) );
   end;

   function C_Raptor_Parser_Get_Locator (Parser: Handle_Type) return RDF.Raptor.Log.Locator_Handle_Type
      with Import, Convention=>C, External_Name=>"raptor_parser_get_locator";

   function Get_Locator (Parser: Parser_Type_Without_Finalize) return RDF.Raptor.Log.Locator_Type is
   begin
      return From_Handle(C_raptor_parser_get_locator(Get_Handle(Parser)));
   end;

   procedure C_Raptor_Parser_Parse_Abort (Parser: Handle_Type)
      with Import, Convention=>C, External_Name=>"raptor_parser_parse_abort";

   procedure Parse_Abort (Parser: Parser_Type_Without_Finalize) is
   begin
      C_Raptor_Parser_Parse_Abort(Get_Handle(Parser));
   end;

   function C_Raptor_Parser_Parse_Chunk (Parser: Handle_Type; Buffer: char_array; Len: size_t; Is_End: int) return int
      with Import, Convention=>C, External_Name=>"raptor_parser_parse_chunk";

   procedure Parse_Chunk (Parser: Parser_Type_Without_Finalize;
                          Buffer: String;
                          Is_End: Boolean) is
   begin
      if C_Raptor_Parser_Parse_Chunk(Get_Handle(Parser),
                                     To_C(Buffer, Append_Nul=>False),
                                     Buffer'Length,
                                     (if Is_End then 1 else 0)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function C_Raptor_Parser_Parse_File (Parser: Handle_Type; URI, Base_URI: RDF.Raptor.URI.Handle_Type) return int
      with Import, Convention=>C, External_Name=>"raptor_parser_parse_file";

   procedure Parse_File (Parser: Parser_Type_Without_Finalize;
                         URI: RDF.Raptor.URI.URI_Type_Without_Finalize;
                         Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize := From_Handle(null)) is
   begin
      if C_Raptor_Parser_Parse_File(Get_Handle(Parser), Get_Handle(URI), Get_Handle(Base_URI)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   procedure Parse_Stdin (Parser: Parser_Type_Without_Finalize;
                          Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize := From_Handle(null)) is
   begin
      Parse_File (Parser, From_Handle(null), Base_URI);
   end;

   function C_Raptor_Parser_Parse_File_Stream (Parser: Handle_Type;
                                               Stream: RDF.Auxiliary.C_File_Access;
                                               Filename: char_array;
                                               Base_URI: RDF.Raptor.URI.Handle_Type) return int
      with Import, Convention=>C, External_Name=>"raptor_parser_parse_file_stream";

   procedure Parse_File_Stream (Parser: Parser_Type_Without_Finalize;
                                Stream: RDF.Auxiliary.C_File_Access;
                                Filename: String;
                                Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize) is
   begin
      if C_Raptor_Parser_Parse_File_Stream(Get_Handle(Parser), Stream, To_C(Filename), Get_Handle(Base_URI)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function C_Raptor_Parser_Parse_Iostream (Parser: Handle_Type;
                                            Stream: RDF.Raptor.IOStream.Handle_Type;
                                            Base_URI: RDF.Raptor.URI.Handle_Type) return int
      with Import, Convention=>C, External_Name=>"raptor_parser_parse_iostream";

   procedure Parse_Iostream (Parser: Parser_Type_Without_Finalize;
                             Stream: RDF.Raptor.IOStream.Base_Stream_Type'Class;
                             Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize) is
      use RDF.Raptor.IOStream;
   begin
      if C_Raptor_Parser_Parse_Iostream(Get_Handle(Parser), Get_Handle(Stream), Get_Handle(Base_URI)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function C_Raptor_Parser_Parse_Start (Parser: Handle_Type; URI: RDF.Raptor.URI.Handle_Type) return int
      with Import, Convention=>C, External_Name=>"raptor_parser_parse_start";

   procedure Parse_Start (Parser: Parser_Type_Without_Finalize; URI: RDF.Raptor.URI.URI_Type_Without_Finalize) is
   begin
      if C_Raptor_Parser_Parse_Start(Get_Handle(Parser), Get_Handle(URI)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function C_Raptor_Parser_Parse_Uri (Parser: Handle_Type;
                                       URI, Base_URI: RDF.Raptor.URI.Handle_Type) return int
      with Import, Convention=>C, External_Name=>"raptor_parser_parse_uri";

   procedure Parse_URI (Parser: Parser_Type_Without_Finalize;
                        URI: RDF.Raptor.URI.URI_Type_Without_Finalize;
                        Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize := From_Handle(null)) is
   begin
      if C_raptor_parser_parse_uri(Get_Handle(Parser), Get_Handle(URI), Get_Handle(Base_URI)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   procedure C_Raptor_Free_Parser (Handle: Handle_Type)
     with Import, Convention=>C, External_Name=>"raptor_free_parser";

   procedure Finalize_Handle (Object: Parser_Type; Handle: Handle_Type) is
   begin
      C_Raptor_Free_Parser(Handle);
   end;

   type User_Defined_Access is access constant Parser_Type_Without_Finalize'Class;
   function Ptr_To_Obj is new Ada.Unchecked_Conversion(chars_ptr, User_Defined_Access);
   function Obj_To_Ptr is new Ada.Unchecked_Conversion(User_Defined_Access, chars_ptr);

   type C_Raptor_Statement_Handler is access procedure (Data: chars_ptr; Statement: RDF.Raptor.Statement.Statement_Handle)
     with Convention=>C;

   type C_Raptor_Graph_Mark_Handler is access procedure (Data: chars_ptr; URI: RDF.Raptor.URI.Handle_Type; Flags: int)
     with Convention=>C;

   type C_Raptor_Namespace_Handler is access procedure (Data: chars_ptr; NS: RDF.Raptor.Namespaces.Namespace_Handle_Type)
     with Convention=>C;

   procedure C_Raptor_Statement_Handler_Impl (Data: Chars_Ptr; Statement: RDF.Raptor.Statement.Statement_Handle)
     with Convention=>C;

   procedure C_Raptor_Graph_Mark_Handler_Impl (Data: chars_ptr; URI: RDF.Raptor.URI.Handle_Type; Flags: int)
     with Convention=>C;

   procedure C_Raptor_Namespace_Handler_Impl (Data: Chars_Ptr; NS: RDF.Raptor.Namespaces.Namespace_Handle_Type)
     with Convention=>C;

   procedure C_Raptor_Statement_Handler_Impl (Data: Chars_Ptr; Statement: RDF.Raptor.Statement.Statement_Handle) is
   begin
      Statement_Handler(Ptr_To_Obj(Data).all, Statement_Type_Without_Finalize'(From_Non_Null_Handle(Statement)));
   end;

   procedure C_Raptor_Graph_Mark_Handler_Impl (Data: chars_ptr; URI: RDF.Raptor.URI.Handle_Type; Flags: int) is
      function Conv is new Ada.Unchecked_Conversion(int, Graph_Mark_Flags);
   begin
      Graph_Mark_Handler(Ptr_To_Obj(Data).all, URI_Type_Without_Finalize'(From_Non_Null_Handle(URI)), Conv(Flags));
   end;

   procedure C_Raptor_Namespace_Handler_Impl (Data: Chars_Ptr; NS: RDF.Raptor.Namespaces.Namespace_Handle_Type) is
   begin
      Namespace_Handler(Ptr_To_Obj(Data).all, Namespace_Type_Without_Finalize'(From_Non_Null_Handle(NS)));
   end;

   procedure Initialize_All_Callbacks (Parser: Parser_Type_Without_Finalize) is
   begin
      Initialize_Graph_Mark_Handler(Parser);
      Initialize_Statement_Handler (Parser);
      Initialize_Namespace_Handler (Parser);
   end;

   procedure C_Raptor_Parser_Set_Statement_Handler (Parser: Handle_Type; Data: chars_ptr; Handler: C_Raptor_Statement_Handler)
      with Import, Convention=>C, External_Name=>"raptor_parser_set_statement_handler";

   procedure C_Raptor_Parser_Set_Graph_Mark_Handler (Parser: Handle_Type; Data: chars_ptr; Handler: C_Raptor_Graph_Mark_Handler)
      with Import, Convention=>C, External_Name=>"raptor_parser_set_graph_mark_handler";

   procedure C_Raptor_Parser_Set_Namespace_Handler (Parser: Handle_Type; Data: chars_ptr; Handler: C_Raptor_Namespace_Handler)
      with Import, Convention=>C, External_Name=>"raptor_parser_set_namespace_handler";

   procedure Initialize_Graph_Mark_Handler (Object: Parser_Type_Without_Finalize) is
   begin
      C_Raptor_Parser_Set_Graph_Mark_Handler(Get_Handle(Object), Obj_To_Ptr(Object'Unchecked_Access), C_Raptor_Graph_Mark_Handler_Impl'Access);
   end;

   procedure Initialize_Statement_Handler (Object: Parser_Type_Without_Finalize) is
   begin
      C_Raptor_Parser_Set_Statement_Handler(Get_Handle(Object), Obj_To_Ptr(Object'Unchecked_Access), C_Raptor_Statement_Handler_Impl'Access);
   end;

   procedure Initialize_Namespace_Handler (Object: Parser_Type_Without_Finalize) is
   begin
      C_Raptor_Parser_Set_Namespace_Handler(Get_Handle(Object), Obj_To_Ptr(Object'Unchecked_Access), C_Raptor_Namespace_Handler_Impl'Access);
   end;

   type My_Dummy_Access is access constant RDF.Auxiliary.Dummy_Record
      with Convention=>C;

   function C_Raptor_Parser_Get_Description (Parser: Handle_Type) return RDF.Raptor.Syntaxes.Syntax_Description_Type
      with Import, Convention=>C, External_Name=>"raptor_parser_get_description";

   function Get_Description (Parser: Parser_Type) return RDF.Raptor.Syntaxes.Syntax_Description_Type is
   begin
      return C_Raptor_Parser_Get_Description(Get_Handle(Parser));
   end;

end RDF.Raptor.Parser;
