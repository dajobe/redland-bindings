with Interfaces.C; use Interfaces.C;

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

   procedure librdf_world_open (Handle: Redland_World_Handle)
     with Import, Convention=>C;

   procedure Open (Object: in out Redland_World_Type_Without_Finalize) is
   begin
      librdf_world_open(Get_Handle(Object));
   end;

   function Open return Redland_World_Type is
   begin
      return Object: Redland_World_Type do
         Open(Object);
      end return;
   end;

   procedure librdf_world_set_rasqal (World: Redland_World_Handle; Rasqal_World: Rasqal_World_Handle)
     with Import, Convention=>C;

   procedure Set_Rasqal (World: Redland_World_Type_Without_Finalize;
                         Rasqal_World: Rasqal_World_Type_Without_Finalize'Class) is
   begin
      librdf_world_set_rasqal(Get_Handle(World), Get_Handle(Rasqal_World));
   end;

   function librdf_world_get_rasqal (World: Redland_World_Handle) return Rasqal_World_Handle
     with Import, Convention=>C;

   function Get_Rasqal (World: Redland_World_Type_Without_Finalize)
                        return Rasqal_World_Type_Without_Finalize is
   begin
      return From_Non_Null_Handle(librdf_world_get_rasqal(Get_Handle(World)));
   end;

   procedure librdf_world_set_raptor (World: Redland_World_Handle; Raptor_World: Raptor_World_Handle)
     with Import, Convention=>C;

   procedure Set_Raptor (World: Redland_World_Type_Without_Finalize;
                         Raptor_World: Raptor_World_Type_Without_Finalize'Class) is
   begin
      librdf_world_set_raptor(Get_Handle(World), Get_Handle(Raptor_World));
   end;

   function librdf_world_get_raptor (World: Redland_World_Handle) return Raptor_World_Handle
     with Import, Convention=>C;

   function Get_Raptor (World: Redland_World_Type_Without_Finalize)
                        return Raptor_World_Type_Without_Finalize is
   begin
      return From_Non_Null_Handle(librdf_world_get_raptor(Get_Handle(World)));
   end;

   procedure librdf_world_set_digest (World: Redland_World_Handle; Name: char_array)
     with Import, Convention=>C;

   procedure Set_Digest (World: Redland_World_Type_Without_Finalize; Name: String) is
   begin
      librdf_world_set_digest(Get_Handle(World), To_C(Name));
   end;

end RDF.Redland.World;
