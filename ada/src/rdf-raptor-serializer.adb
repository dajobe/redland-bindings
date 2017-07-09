with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary;
-- with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;

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

   function raptor_serializer_set_namespace_from_namespace (Serializer: Handle_Type; Namespace: RDF.Raptor.Namespaces.Namespace_Handle_Type)
                                                            return int
      with Import, Convention=>C;

   procedure Set_Namespace (Serializer: Serializer_Type_Without_Finalize;
                            Namespace: RDF.Raptor.Namespaces.Namespace_Type_Without_Finalize) is
      use RDF.Raptor.Namespaces;
   begin
      if raptor_serializer_set_namespace_from_namespace(Get_Handle(Serializer), Get_Handle(Namespace)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_new_serializer (World: RDF.Raptor.World.Handle_Type; Syntax_Name: chars_ptr) return Handle_Type
      with Import, Convention=>C;

   function New_Serializer (World: World_Type) return Serializer_Type is
   begin
      return From_Non_Null_Handle(raptor_new_serializer(Get_Handle(World), Null_Ptr));
   end;

   function New_Serializer (World: World_Type; Syntax_Name: String) return Serializer_Type is
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
