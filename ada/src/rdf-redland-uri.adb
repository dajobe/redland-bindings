with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary;
with RDF.Auxiliary.C_Pointers;
with RDF.Auxiliary.Convert; use RDF.Auxiliary.Convert;
with RDF.Redland.Memory;

package body RDF.Redland.URI is

   function To_Raptor (URI: URI_Type_Without_Finalize'Class) return RDF.Raptor.URI.URI_Type_Without_Finalize is
      use RDF.Raptor.URI;
   begin
      return From_Handle(Get_Handle(URI));
   end;

   function From_Raptor (URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class) return URI_Type_Without_Finalize is
      use RDF.Raptor.URI;
   begin
      return From_Handle(Get_Handle(URI));
   end;

   function librdf_new_uri2 (World: Redland_World_Handle; URI: char_array; Length: size_t)
                             return URI_Handle
     with Import, Convention=>C;

   function From_String (World: Redland_World_Type_Without_Finalize'Class; URI: URI_String)
                         return URI_Type is
      Handle: constant URI_Handle :=
        librdf_new_uri2(Get_Handle(World), To_C(String(URI), Append_Nul=>False), URI'Length);
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_new_uri_from_uri (Old_URI: URI_Handle) return URI_Handle
     with Import, Convention=>C;

   procedure Adjust(Object: in out URI_Type) is
      use RDF.Auxiliary;
   begin
      if Get_Handle(Object) /= null then
         Set_Handle_Hack(Object, librdf_new_uri_from_uri(Get_Handle(Object)));
      end if;
   end;

   function librdf_new_uri_from_uri_local_name (Old_URI: URI_Handle; Local_Name: char_array)
                                                return URI_Handle
     with Import, Convention=>C;

   function From_URI_Local_Name (Old_URI: URI_Type_Without_Finalize'Class;
                                 Local_Name: String)
                                 return URI_Type is
      Handle: constant URI_Handle :=
        librdf_new_uri_from_uri_local_name(Get_Handle(Old_URI), To_C(Local_Name));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   procedure librdf_free_uri (Handle: URI_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle(Object: URI_Type; Handle: URI_Handle) is
   begin
      librdf_free_uri (Handle);
   end;

   type Size_T_P is access all size_t with Convention=>C;

   function librdf_uri_as_counted_string(URI: URI_Handle; Length: Size_T_P)
                                         return RDF.Auxiliary.C_Pointers.Pointer
     with Import, Convention=>C;

   function As_String (URI: URI_Type_Without_Finalize) return String is
      Length: aliased size_t;
      Str: constant RDF.Auxiliary.C_Pointers.Pointer :=
        librdf_uri_as_counted_string(Get_Handle(URI), Length'Unchecked_Access);
   begin
      return Value_With_Possible_NULs(Str, Length);
   end;

   procedure librdf_uri_print (URI: URI_Handle; File: RDF.Auxiliary.C_File_Access)
     with Import, Convention=>C;

   procedure Print (URI: URI_Type_Without_Finalize; File: RDF.Auxiliary.C_File_Access) is
   begin
      librdf_uri_print(Get_Handle(URI), File);
   end;

   function librdf_uri_equals (Left, Right: URI_Handle) return int
     with Import, Convention=>C;

   function Equals (Left, Right: URI_Type_Without_Finalize) return Boolean is
   begin
      return librdf_uri_equals(Get_Handle(Left), Get_Handle(Right)) /= 0;
   end;

   function librdf_uri_is_file_uri (URI: URI_Handle) return int
     with Import, Convention=>C;

   function Is_File_URI (URI: URI_Type_Without_Finalize) return Boolean is
   begin
      return librdf_uri_is_file_uri(Get_Handle(URI)) /= 0;
   end;

   function librdf_uri_to_filename (URI: URI_Handle) return chars_ptr
     with Import, Convention=>C;

   function To_Filename (URI: URI_Type_Without_Finalize) return String is
      Pointer: constant chars_ptr := librdf_uri_to_filename(Get_Handle(URI));
      Result: constant String := Value(Pointer);
   begin
      RDF.Redland.Memory.redland_free_memory(Pointer);
      return Result;
   end;

   function librdf_new_uri_normalised_to_base (URI_Str: char_array; Source_URI, Base_URI: URI_Handle)
                                               return URI_Handle
     with Import, Convention=>C;

   function Normalised_To_Base (URI_Str: URI_String;
                                Source_URI, Base_URI: URI_Type_Without_Finalize'Class)
                                return URI_Type is
      Handle: constant URI_Handle :=
        librdf_new_uri_normalised_to_base(To_C(String(URI_Str)), Get_Handle(Source_URI), Get_Handle(Base_URI));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_new_uri_relative_to_base (Base_URI: URI_Handle; Str: char_array) return URI_Handle
     with Import, Convention=>C;

   function Relative_To_Base (Base_URI: URI_Type_Without_Finalize'Class;
                              Str: URI_String)
                              return URI_Type is
      Handle: constant URI_Handle :=
        librdf_new_uri_relative_to_base(Get_Handle(Base_URI), To_C(String(Str)));
   begin
      return From_Non_Null_Handle(Handle);
   end;

end RDF.Redland.URI;
