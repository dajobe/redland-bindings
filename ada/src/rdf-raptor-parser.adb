with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxilary; use RDF.Auxilary;
with RDF.Auxilary.C_String_Holders; use RDF.Auxilary.C_String_Holders;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Namespaces; use RDF.Raptor.Namespaces;
with RDF.Raptor.Statement; use RDF.Raptor.Statement;

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
   begin
      Graph_Mark_Handler(Ptr_To_Obj(Data).all, URI_Type_Without_Finalize'(From_Non_Null_Handle(URI)), Flags); -- TODO: Use an enumeration type
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

end RDF.Raptor.Parser;
