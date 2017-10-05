package body RDF.Redland.World is

   function librdf_new_world return Redland_World_Handle
     with Import, Convention=>C;

   function Default_Handle (Object: Redland_World_Type_Without_Finalize)
                            return Redland_World_Handle is
   begin
      return librdf_new_world;
   end;

   procedure librdf_free_world (Handle: Redland_World_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Redland_World_Type; Handle: Redland_World_Handle) is
   begin
      librdf_free_world(Handle);
   end;

end RDF.Redland.World;
