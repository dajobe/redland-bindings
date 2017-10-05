with RDF.Auxiliary.Limited_Handled_Record;

package RDF.Redland.World is

   package Redland_World_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Redland_World_Type_Without_Finalize is new Redland_World_Handled_Record.Base_Object with null record;

   subtype Redland_World_Handle is Redland_World_Handled_Record.Access_Type;

   overriding function Default_Handle(Object: Redland_World_Type_Without_Finalize)
                                      return Redland_World_Handle;

   type Redland_World_Type is new Redland_World_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Object: Redland_World_Type; Handle: Redland_World_Handle);

   -- TODO: Stopped at librdf_world_open()

end RDF.Redland.World;
