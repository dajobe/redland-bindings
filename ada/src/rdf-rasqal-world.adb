package body RDF.Rasqal.World is

   procedure rasqal_free_world (World: Handle_Type)
      with Import, Convention=>C;

   procedure Finalize_Handle (Object: World_Type; Handle: Handle_Type) is
   begin
      rasqal_free_world(Handle);
   end;

end RDF.Rasqal.World;
