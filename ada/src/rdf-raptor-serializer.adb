with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary;
-- with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Options;

package body RDF.Raptor.Serializer is

   function raptor_serializer_start_to_iostream (Serializer: Handle_Type;
                                                 URI: RDF.Raptor.URI.Handle_Type;
                                                 Iostream: RDF.Raptor.Iostream.Handle_Type)
                                                 return int
      with Import, Convention=>C;

   procedure Start_To_Iostream (Serializer: Serializer_Type_Without_Finalize;
                                Iostream: RDF.Raptor.Iostream.Base_Stream_Type'Class;
                                URI: URI_Type_Without_Finalize := From_Handle(null)) is
      use RDF.Raptor.Iostream;
   begin
      if raptor_serializer_start_to_iostream(Get_Handle(Serializer), Get_Handle(URI), Get_Handle(Iostream)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_serializer_start_to_filename (Serializer: Handle_Type; Filename: char_array) return int
      with Import, Convention=>C;

   procedure Start_To_Filename (Serializer: Serializer_Type_Without_Finalize; Filename: String) is
   begin
      if raptor_serializer_start_to_filename(Get_Handle(Serializer), To_C(Filename)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_serializer_start_to_file_handle (Serializer: Handle_Type; URI: RDF.Raptor.URI.Handle_Type;  FH: RDF.Auxiliary.C_File_Access)
                                                    return int
      with Import, Convention=>C;

   procedure Start_To_Filehandle (Serializer: Serializer_Type_Without_Finalize;
                                  URI: URI_Type_Without_Finalize'Class;
                                  FH: RDF.Auxiliary.C_File_Access) is
   begin
      if raptor_serializer_start_to_file_handle(Get_Handle(Serializer), Get_Handle(URI), FH) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_serializer_set_namespace (Serializer: Handle_Type; URI: RDF.Raptor.URI.Handle_Type; Prefix: chars_ptr) return int
      with Import, Convention=>C;

   procedure Set_Namespace (Serializer: Serializer_Type_Without_Finalize;
                            Prefix: String;
                            URI: URI_Type_Without_Finalize := From_Handle(null)) is
      V: aliased char_array := To_C(Prefix);
   begin
      if raptor_serializer_set_namespace(Get_Handle(Serializer), Get_Handle(URI), To_Chars_Ptr(V'Unchecked_Access)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   procedure Set_Namespace_Without_Prefix (Serializer: Serializer_Type_Without_Finalize;
                                           URI: URI_Type_Without_Finalize := From_Handle(null)) is
   begin
      if raptor_serializer_set_namespace(Get_Handle(Serializer), Get_Handle(URI), Null_Ptr) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_serializer_set_namespace_from_namespace (Serializer: Handle_Type; Namespace: RDF.Raptor.Namespace.Namespace_Handle_Type)
                                                            return int
      with Import, Convention=>C;

   procedure Set_Namespace (Serializer: Serializer_Type_Without_Finalize;
                            Namespace: RDF.Raptor.Namespace.Namespace_Type_Without_Finalize) is
      use RDF.Raptor.Namespace;
   begin
      if raptor_serializer_set_namespace_from_namespace(Get_Handle(Serializer), Get_Handle(Namespace)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_serializer_serialize_statement (Serializer: Handle_Type; Statement: RDF.Raptor.Statement.Statement_Handle)
                                                   return int
      with Import, Convention=>C;

   procedure Serialize_Statement (Serializer: Serializer_Type_Without_Finalize;
                                  Statement: RDF.Raptor.Statement.Statement_Type_Without_Finalize'Class) is
      use RDF.Raptor.Statement;
   begin
      if raptor_serializer_serialize_statement(Get_Handle(Serializer), Get_Handle(Statement)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_serializer_serialize_end (Serializer: Handle_Type) return int
      with Import, Convention=>C;

   procedure Serialize_End (Serializer: Serializer_Type_Without_Finalize) is
   begin
      if raptor_serializer_serialize_end(Get_Handle(Serializer)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_serializer_serialize_flush (Serializer: Handle_Type) return int
      with Import, Convention=>C;

   procedure Serialize_Flush (Serializer: Serializer_Type_Without_Finalize) is
   begin
      if raptor_serializer_serialize_end(Get_Handle(Serializer)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_serializer_get_description (Serializer: Handle_Type) return RDF.Raptor.Syntaxes.Syntax_Description_Type
      with Import, Convention=>C;

   function Get_Description (Serializer: Serializer_Type_Without_Finalize) return RDF.Raptor.Syntaxes.Syntax_Description_Type is
   begin
      return raptor_serializer_get_description(Get_Handle(Serializer));
   end;

   function raptor_serializer_get_iostream (Serializer: Handle_Type) return RDF.Raptor.Iostream.Handle_Type
      with Import, Convention=>C;

   function Get_Iostream (Serializer: Serializer_Type_Without_Finalize) return RDF.Raptor.Iostream.Stream_Type_Without_Finalize is
      use RDF.Raptor.Iostream;
   begin
      return From_Handle(raptor_serializer_get_iostream(Get_Handle(Serializer)));
   end;

   function raptor_serializer_get_locator (Serializer: Handle_Type) return RDF.Raptor.Log.Locator_Handle_Type
      with Import, Convention=>C;

   function Get_Locator (Serializer: Serializer_Type_Without_Finalize) return RDF.Raptor.Log.Locator_Type is
      use RDF.Raptor.Log;
   begin
      return From_Handle(raptor_serializer_get_locator(Get_Handle(Serializer)));
   end;

   function raptor_parser_set_option (Serializer: Handle_Type; Option: RDF.Raptor.Options.Raptor_Option; Value: chars_ptr; Int_Value: int) return int
      with Import, Convention=>C;

   procedure Set_Option (Serializer: Serializer_Type_Without_Finalize; Option: RDF.Raptor.Options.Raptor_Option; Value: String) is
      Value2: aliased char_array := To_C(Value);
   begin
      if raptor_parser_set_option(Get_Handle(Serializer), Option, To_Chars_Ptr(Value2'Unchecked_Access), 0) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   procedure Set_Option (Serializer: Serializer_Type_Without_Finalize; Option: RDF.Raptor.Options.Raptor_Option; Value: int) is
   begin
      if raptor_parser_set_option(Get_Handle(Serializer), Option, Null_Ptr, Value) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   type String_P_Type is access all chars_ptr with Convention=>C;
   type Int_P_Type is access all int with Convention=>C;

   function raptor_parser_get_option (Serializer: Handle_Type;
                                      Option: RDF.Raptor.Options.Raptor_Option;
                                      String_P: String_P_Type;
                                      Integer_P: Int_P_Type) return int
      with Import, Convention=>C;

   function Get_Numeric_Option (Serializer: Serializer_Type_Without_Finalize; Option: RDF.Raptor.Options.Raptor_Option) return Natural is
      V: aliased int;
   begin
      if raptor_parser_get_option(Get_Handle(Serializer), Option, null, V'Unchecked_Access) < 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Natural(V);
   end;

   function Get_String_Option (Serializer: Serializer_Type_Without_Finalize; Option: RDF.Raptor.Options.Raptor_Option) return String is
      V: aliased chars_ptr;
   begin
      if raptor_parser_get_option(Get_Handle(Serializer), Option, V'Unchecked_Access, null) < 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Value(V); -- do NOT free it
   end;

   function raptor_serializer_get_world (Serializer: Handle_Type) return RDF.Raptor.World.Handle_Type
      with Import, Convention=>C;

   function Get_World (Serializer: Serializer_Type_Without_Finalize) return Raptor_World_Type_Without_Finalize is
   begin
      return From_Handle(raptor_serializer_get_world(Get_Handle(Serializer)));
   end;

   function raptor_new_serializer (World: RDF.Raptor.World.Handle_Type; Syntax_Name: chars_ptr) return Handle_Type
      with Import, Convention=>C;

   function New_Serializer (World: Raptor_World_Type) return Serializer_Type is
   begin
      return From_Non_Null_Handle(raptor_new_serializer(Get_Handle(World), Null_Ptr));
   end;

   function New_Serializer (World: Raptor_World_Type; Syntax_Name: String) return Serializer_Type is
      V: aliased char_array := To_C(Syntax_Name);
   begin
      return From_Non_Null_Handle(raptor_new_serializer(Get_Handle(World), To_Chars_Ptr(V'Unchecked_Access)));
   end;

   procedure raptor_free_serializer (Serializer: Handle_Type)
      with Import, Convention=>C;

   procedure Finalize_Handle (Serializer: Serializer_Type; Handle: Handle_Type) is
   begin
      raptor_free_serializer(Handle);
   end;

end RDF.Raptor.Serializer;
