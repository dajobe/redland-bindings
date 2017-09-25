with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary; use RDF.Auxiliary;
with RDF.Auxiliary.C_String_Holders; use RDF.Auxiliary.C_String_Holders;
with RDF.Raptor.Memory;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.Namespace; use RDF.Raptor.Namespace;
with RDF.Raptor.Statement; use RDF.Raptor.Statement;
with RDF.Raptor.Log; use RDF.Raptor.Log;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.IOStream; use RDF.Raptor.IOStream;
with RDF.Auxiliary.Convert; use RDF.Auxiliary.Convert;

package body RDF.Raptor.Parser is

--     use all type Parser_Type;

   function raptor_new_parser (World: Raptor_World_Handle; Name: char_array) return Parser_Handle
     with Import, Convention=>C;

   function Create (World: Raptor_World_Type_Without_Finalize'Class; Name: String) return Parser_Type is
   begin
      return From_Non_Null_Handle( raptor_new_parser(Get_Handle(World), To_C(Name, Append_Nul=>True)) );
   end;

   function raptor_new_parser_for_content (World: Raptor_World_Handle;
                                             URI: URI_Handle;
                                             MIME_Type: chars_ptr;
                                             Buffer: chars_ptr;
                                             Len: size_t;
                                             Identifier: chars_ptr) return Parser_Handle
     with Import, Convention=>C;

   function Create_From_Content (World: Raptor_World_Type_Without_Finalize'Class;
                                 URI: URI_Type_Without_Finalize'Class;
                                 Mime_Type : String_Holders.Holder;
                                 Buffer    : String_Holders.Holder;
                                 Identifier: String_Holders.Holder)
                                 return Parser_Type is
      Mime_Type_N : C_String_Holder := To_C_String_Holder(Mime_Type);
      Buffer_N    : C_String_Holder := To_C_String_Holder(Buffer);
      Identifier_N: C_String_Holder := To_C_String_Holder(Identifier);
   begin
      return  From_Non_Null_Handle( raptor_new_parser_for_content(Get_Handle(World),
                                                                    Get_Handle(URI),
                                                                    C_String(Mime_Type_N),
                                                                    C_String(Buffer_N),
                                                                    size_t(Length(Buffer)),
                                                                    C_String(Identifier_N)) );
   end;

   function raptor_parser_get_locator (Parser: Parser_Handle) return Locator_Handle
      with Import, Convention=>C;

   function Get_Locator (Parser: Parser_Type_Without_Finalize) return Locator_Type is
   begin
      return From_Handle(raptor_parser_get_locator(Get_Handle(Parser)));
   end;

   procedure raptor_parser_parse_abort (Parser: Parser_Handle)
      with Import, Convention=>C;

   procedure Parse_Abort (Parser: Parser_Type_Without_Finalize) is
   begin
      raptor_parser_parse_abort(Get_Handle(Parser));
   end;

   function raptor_parser_parse_chunk (Parser: Parser_Handle; Buffer: char_array; Len: size_t; Is_End: int) return int
      with Import, Convention=>C;

   procedure Parse_Chunk (Parser: Parser_Type_Without_Finalize;
                          Buffer: String;
                          Is_End: Boolean) is
   begin
      if raptor_parser_parse_chunk(Get_Handle(Parser),
                                   My_To_C_Without_Nul(Buffer),
                                   Buffer'Length,
                                   (if Is_End then 1 else 0)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_parser_parse_file (Parser: Parser_Handle; URI, Base_URI: URI_Handle) return int
      with Import, Convention=>C;

   procedure Parse_File (Parser: Parser_Type_Without_Finalize;
                         URI: URI_Type_Without_Finalize;
                         Base_URI: URI_Type_Without_Finalize := From_Handle(null)) is
   begin
      if raptor_parser_parse_file(Get_Handle(Parser), Get_Handle(URI), Get_Handle(Base_URI)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   procedure Parse_Stdin (Parser: Parser_Type_Without_Finalize;
                          Base_URI: URI_Type_Without_Finalize := From_Handle(null)) is
   begin
      Parse_File (Parser, From_Handle(null), Base_URI);
   end;

   function raptor_parser_parse_file_stream (Parser: Parser_Handle;
                                               Stream: RDF.Auxiliary.C_File_Access;
                                               Filename: char_array;
                                               Base_URI: URI_Handle) return int
      with Import, Convention=>C;

   procedure Parse_File_Stream (Parser: Parser_Type_Without_Finalize;
                                Stream: RDF.Auxiliary.C_File_Access;
                                Filename: String;
                                Base_URI: URI_Type_Without_Finalize) is
   begin
      if raptor_parser_parse_file_stream(Get_Handle(Parser), Stream, To_C(Filename), Get_Handle(Base_URI)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_parser_parse_iostream (Parser: Parser_Handle;
                                            Stream: IOStream_Handle;
                                            Base_URI: URI_Handle) return int
      with Import, Convention=>C;

   procedure Parse_Iostream (Parser: Parser_Type_Without_Finalize;
                             Stream: Base_Stream_Type'Class;
                             Base_URI: URI_Type_Without_Finalize) is
      use RDF.Raptor.IOStream;
   begin
      if raptor_parser_parse_iostream(Get_Handle(Parser), Get_Handle(Stream), Get_Handle(Base_URI)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_parser_parse_start (Parser: Parser_Handle; URI: URI_Handle) return int
      with Import, Convention=>C;

   procedure Parse_Start (Parser: Parser_Type_Without_Finalize; URI: URI_Type_Without_Finalize) is
   begin
      if raptor_parser_parse_start(Get_Handle(Parser), Get_Handle(URI)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_parser_parse_uri (Parser: Parser_Handle;
                                     URI, Base_URI: URI_Handle) return int
      with Import, Convention=>C;

   procedure Parse_URI (Parser: Parser_Type_Without_Finalize;
                        URI: URI_Type_Without_Finalize;
                        Base_URI: URI_Type_Without_Finalize := From_Handle(null)) is
   begin
      if raptor_parser_parse_uri(Get_Handle(Parser), Get_Handle(URI), Get_Handle(Base_URI)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_parser_parse_uri_with_connection (Parser: Parser_Handle;
                                                     URI, Base_URI: URI_Handle;
                                                     Connection: RDF.Raptor.WWW.Connection_Type)
                                                     return int
      with Import, Convention=>C;

   procedure Parse_URI_With_Connection (Parser: Parser_Type_Without_Finalize;
                                        URI: URI_Type_Without_Finalize;
                                        Base_URI: URI_Type_Without_Finalize := From_Handle(null);
                                        Connection: RDF.Raptor.WWW.Connection_Type := null) is
   begin
      if raptor_parser_parse_uri_with_connection(Get_Handle(Parser), Get_Handle(URI), Get_Handle(Base_URI), Connection) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_parser_get_graph (Parser: Parser_Handle) return URI_Handle
         with Import, Convention=>C;

   function Get_Graph (Parser: Parser_Type_Without_Finalize) return URI_Type is
   begin
      return From_Handle(raptor_parser_get_graph(Get_Handle(Parser)));
   end;

   function raptor_parser_get_description (Parser: Parser_Handle) return RDF.Raptor.Syntaxes.Syntax_Description_Type
      with Import, Convention=>C;

   function Get_Description (Parser: Parser_Type_Without_Finalize) return RDF.Raptor.Syntaxes.Syntax_Description_Type is
   begin
      return raptor_parser_get_description(Get_Handle(Parser));
   end;

   function raptor_parser_get_name (Parser: Parser_Handle) return chars_ptr
         with Import, Convention=>C;

   function Get_Name (Parser: Parser_Type_Without_Finalize) return String is
   begin
      return Value(raptor_parser_get_name(Get_Handle(Parser)));
   end;

   function raptor_parser_set_option (Parser: Parser_Handle; Option: RDF.Raptor.Options.Raptor_Option; Value: chars_ptr; Int_Value: int) return int
      with Import, Convention=>C;

   procedure Set_Option (Parser: Parser_Type_Without_Finalize; Option: RDF.Raptor.Options.Raptor_Option; Value: String) is
      Value2: aliased char_array := To_C(Value);
   begin
      if raptor_parser_set_option(Get_Handle(Parser), Option, To_Chars_Ptr(Value2'Unchecked_Access), 0) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   procedure Set_Option (Parser: Parser_Type_Without_Finalize; Option: RDF.Raptor.Options.Raptor_Option; Value: int) is
   begin
      if raptor_parser_set_option(Get_Handle(Parser), Option, Null_Ptr, Value) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   type String_P_Type is access all chars_ptr with Convention=>C;
   type Int_P_Type is access all int with Convention=>C;

   function raptor_parser_get_option (Parser: Parser_Handle;
                                      Option: RDF.Raptor.Options.Raptor_Option;
                                      String_P: String_P_Type;
                                      Integer_P: Int_P_Type) return int
      with Import, Convention=>C;

   function Get_Numeric_Option (Parser: Parser_Type_Without_Finalize; Option: RDF.Raptor.Options.Raptor_Option) return Natural is
      V: aliased int;
   begin
      if raptor_parser_get_option(Get_Handle(Parser), Option, null, V'Unchecked_Access) < 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Natural(V);
   end;

   function Get_String_Option (Parser: Parser_Type_Without_Finalize; Option: RDF.Raptor.Options.Raptor_Option) return String is
      V: aliased chars_ptr;
   begin
      if raptor_parser_get_option(Get_Handle(Parser), Option, V'Unchecked_Access, null) < 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Value(V); -- do NOT free it
   end;

   function raptor_parser_get_accept_header (Parser: Parser_Handle) return chars_ptr
      with Import, Convention=>C;

   function Get_Accept_Header (Parser: Parser_Type_Without_Finalize) return String is
      V: constant chars_ptr := raptor_parser_get_accept_header(Get_Handle(Parser));
      S: constant String := Value(V);
   begin
      RDF.Raptor.Memory.raptor_free_memory(V);
      return S;
   end;

   function raptor_parser_get_world (Parser: Parser_Handle) return Raptor_World_Handle
      with Import, Convention=>C;

   function Get_World (Parser: Parser_Type_Without_Finalize) return Raptor_World_Type_Without_Finalize is
   begin
      return From_Handle(raptor_parser_get_world(Get_Handle(Parser)));
   end;

   procedure raptor_free_parser (Handle: Parser_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Parser_Type; Handle: Parser_Handle) is
   begin
      raptor_free_parser(Handle);
   end;

   type User_Defined_Access is access constant Parser_Type_Without_Finalize'Class;
   function Ptr_To_Obj is new Ada.Unchecked_Conversion(chars_ptr, User_Defined_Access);
   function Obj_To_Ptr is new Ada.Unchecked_Conversion(User_Defined_Access, chars_ptr);

   type raptor_statement_handler is access procedure (Data: chars_ptr; Statement: Statement_Handle)
     with Convention=>C;

   type raptor_graph_mark_handler is access procedure (Data: chars_ptr; URI: URI_Handle; Flags: int)
     with Convention=>C;

   type raptor_namespace_handler is access procedure (Data: chars_ptr; NS: Namespace_Handle)
     with Convention=>C;

   type raptor_uri_filter_func is access function (Data: chars_ptr; URI: URI_Handle) return int
     with Convention=>C;

   procedure raptor_statement_handler_impl (Data: Chars_Ptr; Statement: Statement_Handle)
     with Convention=>C;

   procedure raptor_graph_mark_handler_impl (Data: chars_ptr; URI: URI_Handle; Flags: int)
     with Convention=>C;

   procedure raptor_namespace_handler_impl (Data: Chars_Ptr; NS: Namespace_Handle)
     with Convention=>C;

   function raptor_uri_filter_impl (Data: chars_ptr; URI: URI_Handle) return int
     with Convention=>C;

   procedure raptor_statement_handler_impl (Data: Chars_Ptr; Statement: Statement_Handle) is
   begin
      Statement_Handler(Ptr_To_Obj(Data).all, Statement_Type_Without_Finalize'(From_Non_Null_Handle(Statement)));
   end;

   procedure raptor_graph_mark_handler_impl (Data: chars_ptr; URI: URI_Handle; Flags: int) is
      function Conv is new Ada.Unchecked_Conversion(int, Graph_Mark_Flags);
   begin
      Graph_Mark_Handler(Ptr_To_Obj(Data).all, URI_Type_Without_Finalize'(From_Non_Null_Handle(URI)), Conv(Flags));
   end;

   procedure raptor_namespace_handler_impl (Data: Chars_Ptr; NS: Namespace_Handle) is
   begin
      Namespace_Handler(Ptr_To_Obj(Data).all, Namespace_Type_Without_Finalize'(From_Non_Null_Handle(NS)));
   end;

   function raptor_uri_filter_impl (Data: chars_ptr; URI: URI_Handle) return int is
   begin
      return (if URI_Filter(Ptr_To_Obj(Data).all, URI_Type_Without_Finalize'(From_Non_Null_Handle(URI))) then 1 else 0);
   end;

   procedure Initialize_All_Callbacks (Parser: Parser_Type_Without_Finalize) is
   begin
      Initialize_Graph_Mark_Handler(Parser);
      Initialize_Statement_Handler (Parser);
      Initialize_Namespace_Handler (Parser);
      Initialize_URI_Filter        (Parser);
   end;

   procedure raptor_parser_set_statement_handler (Parser: Parser_Handle; Data: chars_ptr; Handler: raptor_statement_handler)
      with Import, Convention=>C;

   procedure raptor_parser_set_graph_mark_handler (Parser: Parser_Handle; Data: chars_ptr; Handler: raptor_graph_mark_handler)
      with Import, Convention=>C;

   procedure raptor_parser_set_namespace_handler (Parser: Parser_Handle; Data: chars_ptr; Handler: raptor_namespace_handler)
      with Import, Convention=>C;

   procedure raptor_parser_set_uri_filter (Parser: Parser_Handle; Handler: raptor_uri_filter_func; Data: chars_ptr)
      with Import, Convention=>C;

   procedure Initialize_Graph_Mark_Handler (Object: Parser_Type_Without_Finalize) is
   begin
      raptor_parser_set_graph_mark_handler(Get_Handle(Object), Obj_To_Ptr(Object'Unchecked_Access), raptor_graph_mark_handler_impl'Access);
   end;

   procedure Initialize_Statement_Handler (Object: Parser_Type_Without_Finalize) is
   begin
      raptor_parser_set_statement_handler(Get_Handle(Object), Obj_To_Ptr(Object'Unchecked_Access), raptor_statement_handler_impl'Access);
   end;

   procedure Initialize_Namespace_Handler (Object: Parser_Type_Without_Finalize) is
   begin
      raptor_parser_set_namespace_handler(Get_Handle(Object), Obj_To_Ptr(Object'Unchecked_Access), raptor_namespace_handler_impl'Access);
   end;

   procedure Initialize_URI_Filter (Object: Parser_Type_Without_Finalize) is
   begin
      raptor_parser_set_uri_filter(Get_Handle(Object), raptor_uri_filter_impl'Access, Obj_To_Ptr(Object'Unchecked_Access));
   end;

   type My_Dummy_Access is access constant RDF.Auxiliary.Dummy_Record
      with Convention=>C;

end RDF.Raptor.Parser;
