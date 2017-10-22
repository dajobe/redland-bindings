with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.Convert; use RDF.Auxiliary.Convert;
with RDF.Redland.Memory;

package body RDF.Redland.Serializer is

   function librdf_serializer_get_description (Serializer: Serializer_Handle; Index: unsigned)
                                               return Raptor_Syntax_Description_Type
     with Import, Convention=>C;

   function Get_Description (Serializer: Serializer_Type_Without_Finalize; Index: Natural)
                             return Raptor_Syntax_Description_Type is
   begin
      return librdf_serializer_get_description(Get_Handle(Serializer), unsigned(Index));
   end;

   function librdf_new_serializer (World: Redland_World_Handle;
                                   Name, Mime_Type: chars_ptr;
                                   Type_URI: URI_Handle)
                                   return Serializer_Handle
     with Import, Convention=>C;

   function Create (World: Redland_World_Type_Without_Finalize'Class;
                    Name, Mime_Type: String := "";
                    Type_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                    return Serializer_Type is
      Name2: aliased char_array := To_C(Name);
      Mime_Type2: aliased char_array := To_C(Mime_Type);
      Handle: constant Serializer_Handle :=
        librdf_new_serializer(Get_Handle(World),
                              (if Name = "" then Null_Ptr else To_Chars_Ptr(Name2'Unchecked_Access)),
                              (if Mime_Type = "" then Null_Ptr else To_Chars_Ptr(Mime_Type2'Unchecked_Access)),
                              Get_Handle(Type_URI));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   procedure librdf_free_serializer (Serializer: Serializer_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Serializer_Type_Without_Finalize; Handle: Serializer_Handle) is
   begin
      librdf_free_serializer(Handle);
   end;

   function librdf_serializer_check_name (World: Redland_World_Handle; Name: char_array)
                                          return int
     with Import, Convention=>C;

   function Serializer_Check_Name (World: Redland_World_Type_Without_Finalize'Class;
                                   Name: String)
                                   return Boolean is
   begin
      return librdf_serializer_check_name(Get_Handle(World), To_C(Name)) /= 0;
   end;

   function librdf_serializer_serialize_model_to_file_handle (Serializer: Serializer_Handle;
                                                              File: RDF.Auxiliary.C_File_Access;
                                                              Base_URI: URI_Handle;
                                                              Model: Model_Handle)
                                                              return int
     with Import, Convention=>C;

   procedure Serialize_To_File_Handle (Serializer: Serializer_Type_Without_Finalize;
                                       File: RDF.Auxiliary.C_File_Access;
                                       Model: Model_Type_Without_Finalize'Class;
                                       Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null))) is
   begin
      if librdf_serializer_serialize_model_to_file_handle(Get_Handle(Serializer),
                                                          File,
                                                          Get_Handle(Base_URI),
                                                          Get_Handle(Model)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_serializer_serialize_model_to_file (Serializer: Serializer_Handle;
                                                       File_Name: char_array;
                                                       Base_URI: URI_Handle;
                                                       Model: Model_Handle)
                                                       return int
     with Import, Convention=>C;

   procedure Serialize_To_File (Serializer: Serializer_Type_Without_Finalize;
                                File_Name: String;
                                Model: Model_Type_Without_Finalize'Class;
                                Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null))) is
   begin
      if librdf_serializer_serialize_model_to_file(Get_Handle(Serializer),
                                                   To_C(File_Name),
                                                   Get_Handle(Base_URI),
                                                   Get_Handle(Model)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   type Size_T_P is access all size_t with Convention=>C;

   function librdf_serializer_serialize_model_to_counted_string (Serialize: Serializer_Handle;
                                                                 Base_URI: URI_Handle;
                                                                 Model: Model_Handle;
                                                                 Length: Size_T_P)
                                                                 return chars_ptr
        with Import, Convention=>C;

   function Serialize_To_String (Serializer: Serializer_Type_Without_Finalize;
                                 Model: Model_Type_Without_Finalize'Class;
                                 Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                                 return String is
      Length: aliased size_t;
      Ptr: constant chars_ptr :=
        librdf_serializer_serialize_model_to_counted_string(Get_Handle(Serializer),
                                                            Get_Handle(Base_URI),
                                                            Get_Handle(Model),
                                                            Length'Unchecked_Access);
   begin
      if Ptr = Null_Ptr then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      declare
         Str: constant String := Value_With_Possible_NULs(Convert(Ptr), Length);
      begin
         RDF.Redland.Memory.librdf_free_memory(Ptr);
         return Str;
      end;
   end;

   function librdf_serializer_serialize_model_to_iostream (Serializer: Serializer_Handle;
                                                           Base_URI: URI_Handle;
                                                           Model: Model_Handle;
                                                           IOStream: IOStream_Handle)
                                                           return int
     with Import, Convention=>C;

   procedure Serialize_Model_To_IOStream
     (Serializer: Serializer_Type_Without_Finalize;
      Model: Model_Type_Without_Finalize'Class;
      IOStream: Base_IOStream_Type'Class;
      Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null))) is
   begin
      if librdf_serializer_serialize_model_to_iostream(Get_Handle(Serializer),
                                                       Get_Handle(Base_URI),
                                                       Get_Handle(Model),
                                                       Get_Handle(IOStream)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_serializer_serialize_stream_to_counted_string (Serializer: Serializer_Handle;
                                                                  Base_URI: URI_Handle;
                                                                  Stream: IOStream_Handle;
                                                                  Length: Size_T_P)
                                                                  return chars_ptr
     with Import, Convention=>C;

   function Serialize_To_String (Serializer: Serializer_Type_Without_Finalize;
                                 Stream: Stream_Type_Without_Finalize'Class;
                                 Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                                 return String is
      Length: aliased size_t;
      Ptr: constant chars_ptr :=
        librdf_serializer_serialize_stream_to_counted_string(Get_Handle(Serializer),
                                                             Get_Handle(Base_URI),
                                                             Get_Handle(Stream),
                                                             Length'Unchecked_Access);
   begin
      if Ptr = Null_Ptr then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      declare
         Str: constant String := Value_With_Possible_NULs(Convert(Ptr), Length);
      begin
         RDF.Redland.Memory.librdf_free_memory(Ptr);
         return Str;
      end;
   end;

   function librdf_serializer_serialize_stream_to_file (Serializer: Serializer_Handle;
                                                        File_Name: char_array;
                                                        Base_URI: URI_Handle;
                                                        Stream: Stream_Handle)
                                                        return int
     with Import, Convention=>C;

   procedure Serialize_To_File (Serializer: Serializer_Type_Without_Finalize;
                                File_Name: String;
                                Stream: Stream_Type_Without_Finalize'Class;
                                Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null))) is
   begin
      if librdf_serializer_serialize_stream_to_file(Get_Handle(Serializer),
                                                    To_C(File_Name),
                                                    Get_Handle(Base_URI),
                                                    Get_Handle(Stream)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_serializer_serialize_stream_to_file_handle (Serializer: Serializer_Handle;
                                                               File: RDF.Auxiliary.C_File_Access;
                                                               Base_URI: URI_Handle;
                                                               Stream: Stream_Handle)
                                                               return int
     with Import, Convention=>C;

   procedure Serialize_To_File_Handle (Serializer: Serializer_Type_Without_Finalize;
                                       File: RDF.Auxiliary.C_File_Access;
                                       Stream: Stream_Type_Without_Finalize'Class;
                                       Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null))) is
   begin
      if librdf_serializer_serialize_stream_to_file_handle(Get_Handle(Serializer),
                                                           File,
                                                           Get_Handle(Base_URI),
                                                           Get_Handle(Stream)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function librdf_serializer_serialize_stream_to_iostream (Serializer: Serializer_Handle;
                                                            Base_URI: URI_Handle;
                                                            Stream: Stream_Handle;
                                                            File: IOStream_Handle)
                                                            return int
     with Import, Convention=>C;

   procedure Serialize_To_IOStream (Serializer: Serializer_Type_Without_Finalize;
                                    File: Base_IOStream_Type'Class;
                                    Stream: Stream_Type_Without_Finalize'Class;
                                    Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null))) is
   begin
      if librdf_serializer_serialize_stream_to_iostream(Get_Handle(Serializer),
                                                        Get_Handle(File),
                                                        Get_Handle(Base_URI),
                                                        Get_Handle(Stream)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

end RDF.Redland.Serializer;
