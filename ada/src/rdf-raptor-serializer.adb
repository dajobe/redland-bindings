with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary;

package body RDF.Raptor.Serializer is

   function raptor_serializer_start_to_iostream (Serializer: Serializer_Handle;
                                                 URI: URI_Handle;
                                                 Iostream: IOStream_Handle)
                                                 return int
      with Import, Convention=>C;

   procedure Start_To_Iostream (Serializer: Serializer_Type_Without_Finalize;
                                Iostream: Base_Stream_Type'Class;
                                URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null))) is
   begin
      if raptor_serializer_start_to_iostream(Get_Handle(Serializer), Get_Handle(URI), Get_Handle(Iostream)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_serializer_start_to_filename (Serializer: Serializer_Handle; Filename: char_array) return int
      with Import, Convention=>C;

   procedure Start_To_Filename (Serializer: Serializer_Type_Without_Finalize; Filename: String) is
   begin
      if raptor_serializer_start_to_filename(Get_Handle(Serializer), To_C(Filename)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_serializer_start_to_file_handle (Serializer: Serializer_Handle; URI: URI_Handle;  FH: RDF.Auxiliary.C_File_Access)
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

   function raptor_serializer_set_namespace (Serializer: Serializer_Handle; URI: URI_Handle; Prefix: chars_ptr) return int
      with Import, Convention=>C;

   procedure Set_Namespace (Serializer: Serializer_Type_Without_Finalize;
                            Prefix: String;
                            URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null))) is
      V: aliased char_array := To_C(Prefix);
   begin
      if raptor_serializer_set_namespace(Get_Handle(Serializer), Get_Handle(URI), To_Chars_Ptr(V'Unchecked_Access)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   procedure Set_Namespace_Without_Prefix (Serializer: Serializer_Type_Without_Finalize;
                                           URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null))) is
   begin
      if raptor_serializer_set_namespace(Get_Handle(Serializer), Get_Handle(URI), Null_Ptr) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_serializer_set_namespace_from_namespace (Serializer: Serializer_Handle; Namespace: Namespace_Handle)
                                                            return int
      with Import, Convention=>C;

   procedure Set_Namespace (Serializer: Serializer_Type_Without_Finalize;
                            Namespace: Namespace_Type_Without_Finalize'Class) is
   begin
      if raptor_serializer_set_namespace_from_namespace(Get_Handle(Serializer), Get_Handle(Namespace)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_serializer_serialize_statement (Serializer: Serializer_Handle; Statement: Statement_Handle)
                                                   return int
      with Import, Convention=>C;

   procedure Serialize_Statement (Serializer: Serializer_Type_Without_Finalize;
                                  Statement: Statement_Type_Without_Finalize'Class) is
   begin
      if raptor_serializer_serialize_statement(Get_Handle(Serializer), Get_Handle(Statement)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_serializer_serialize_end (Serializer: Serializer_Handle) return int
      with Import, Convention=>C;

   procedure Serialize_End (Serializer: Serializer_Type_Without_Finalize) is
   begin
      if raptor_serializer_serialize_end(Get_Handle(Serializer)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_serializer_serialize_flush (Serializer: Serializer_Handle) return int
      with Import, Convention=>C;

   procedure Serialize_Flush (Serializer: Serializer_Type_Without_Finalize) is
   begin
      if raptor_serializer_serialize_flush(Get_Handle(Serializer)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_serializer_get_description (Serializer: Serializer_Handle) return Raptor_Syntax_Description_Type
      with Import, Convention=>C;

   function Get_Description (Serializer: Serializer_Type_Without_Finalize) return Raptor_Syntax_Description_Type is
   begin
      return raptor_serializer_get_description(Get_Handle(Serializer));
   end;

   function raptor_serializer_get_iostream (Serializer: Serializer_Handle) return IOStream_Handle
      with Import, Convention=>C;

   function Get_Iostream (Serializer: Serializer_Type_Without_Finalize) return Stream_Type_Without_Finalize is
   begin
      return From_Handle(raptor_serializer_get_iostream(Get_Handle(Serializer)));
   end;

   function raptor_serializer_get_locator (Serializer: Serializer_Handle) return Locator_Handle
      with Import, Convention=>C;

   function Get_Locator (Serializer: Serializer_Type_Without_Finalize) return Locator_Type is
   begin
      return From_Handle(raptor_serializer_get_locator(Get_Handle(Serializer)));
   end;

   function raptor_parser_set_option (Serializer: Serializer_Handle; Option: Raptor_Option; Value: chars_ptr; Int_Value: int) return int
      with Import, Convention=>C;

   procedure Set_Option (Serializer: Serializer_Type_Without_Finalize; Option: Raptor_Option; Value: String) is
      Value2: aliased char_array := To_C(Value);
   begin
      if raptor_parser_set_option(Get_Handle(Serializer), Option, To_Chars_Ptr(Value2'Unchecked_Access), 0) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   procedure Set_Option (Serializer: Serializer_Type_Without_Finalize; Option: Raptor_Option; Value: int) is
   begin
      if raptor_parser_set_option(Get_Handle(Serializer), Option, Null_Ptr, Value) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   type String_P_Type is access all chars_ptr with Convention=>C;
   type Int_P_Type is access all int with Convention=>C;

   function raptor_parser_get_option (Serializer: Serializer_Handle;
                                      Option: Raptor_Option;
                                      String_P: String_P_Type;
                                      Integer_P: Int_P_Type) return int
      with Import, Convention=>C;

   function Get_Numeric_Option (Serializer: Serializer_Type_Without_Finalize; Option: Raptor_Option) return Natural is
      V: aliased int;
   begin
      if raptor_parser_get_option(Get_Handle(Serializer), Option, null, V'Unchecked_Access) < 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Natural(V);
   end;

   function Get_String_Option (Serializer: Serializer_Type_Without_Finalize; Option: Raptor_Option) return String is
      V: aliased chars_ptr;
   begin
      if raptor_parser_get_option(Get_Handle(Serializer), Option, V'Unchecked_Access, null) < 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Value(V); -- do NOT free it
   end;

   function raptor_serializer_get_world (Serializer: Serializer_Handle) return Raptor_World_Handle
      with Import, Convention=>C;

   function Get_World (Serializer: Serializer_Type_Without_Finalize) return Raptor_World_Type_Without_Finalize is
   begin
      return From_Handle(raptor_serializer_get_world(Get_Handle(Serializer)));
   end;

   function raptor_new_serializer (World: Raptor_World_Handle; Syntax_Name: chars_ptr) return Serializer_Handle
      with Import, Convention=>C;

   function New_Serializer (World: Raptor_World_Type_Without_Finalize'Class) return Serializer_Type is
   begin
      return From_Non_Null_Handle(raptor_new_serializer(Get_Handle(World), Null_Ptr));
   end;

   function New_Serializer (World: Raptor_World_Type_Without_Finalize'Class; Syntax_Name: String) return Serializer_Type is
      V: aliased char_array := To_C(Syntax_Name);
   begin
      return From_Non_Null_Handle(raptor_new_serializer(Get_Handle(World), To_Chars_Ptr(V'Unchecked_Access)));
   end;

   procedure raptor_free_serializer (Serializer: Serializer_Handle)
      with Import, Convention=>C;

   procedure Finalize_Handle (Serializer: Serializer_Type; Handle: Serializer_Handle) is
   begin
      raptor_free_serializer(Handle);
   end;

end RDF.Raptor.Serializer;
