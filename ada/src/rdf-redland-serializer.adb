with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

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

end RDF.Redland.Serializer;
