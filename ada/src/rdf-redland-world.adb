with Interfaces.C; use Interfaces.C;
with RDF.Redland.Node; use RDF.Redland.Node;
with RDF.Redland.URI; use RDF.Redland.URI;

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

   procedure Finalize_Handle (Object: Redland_World_Type_Without_Finalize; Handle: Redland_World_Handle) is
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

   function librdf_world_get_feature (World: Redland_World_Handle; Feature: URI_Handle) return Node_Handle
     with Import, Convention=>C;

   function Get_Feature (World: Redland_World_Type_Without_Finalize;
                         Feature: RDF.Redland.URI.URI_Type_Without_Finalize'Class)
                         return RDF.Redland.Node.Node_Type is
   begin
      return From_Handle(librdf_world_get_feature(Get_Handle(World), Get_Handle(Feature)));
   end;

   function librdf_world_set_feature (World: Redland_World_Handle; Feature: URI_Handle; Value: Node_Handle)
                                      return int
     with Import, Convention=>C;

   procedure Set_Feature (World: Redland_World_Type_Without_Finalize;
                          Feature: RDF.Redland.URI.URI_Type_Without_Finalize'Class;
                          Value: RDF.Redland.Node.Node_Type_Without_Finalize'Class) is
   begin
      if librdf_world_set_feature(Get_Handle(World), Get_Handle(Feature), Get_Handle(Value)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

end RDF.Redland.World;
