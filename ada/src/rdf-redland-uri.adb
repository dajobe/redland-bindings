with Interfaces.C; use Interfaces.C;

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

end RDF.Redland.URI;
