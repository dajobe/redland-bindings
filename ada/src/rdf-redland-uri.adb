with Interfaces.C; use Interfaces.C;
with RDF.Auxiliary;
with RDF.Auxiliary.C_Pointers;
with RDF.Auxiliary.Convert; use RDF.Auxiliary.Convert;

package body RDF.Redland.URI is

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

end RDF.Redland.URI;
