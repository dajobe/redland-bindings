with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Rasqal.World; use RDF.Rasqal.World;
limited with RDF.Redland.Node;
limited with RDF.Redland.URI;

package RDF.Redland.World is

   Feature_Genid_Base: constant String := "http://feature.librdf.org/genid-base";
   Feature_Genid_Counter: constant String := "http://feature.librdf.org/genid-counter";

   package Redland_World_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Redland_World_Type_Without_Finalize is new Redland_World_Handled_Record.Base_Object with null record;

   subtype Redland_World_Handle is Redland_World_Handled_Record.Access_Type;

   overriding procedure Finalize_Handle (Object: Redland_World_Type_Without_Finalize; Handle: Redland_World_Handle);

   overriding function Default_Handle(Object: Redland_World_Type_Without_Finalize)
                                      return Redland_World_Handle;

   not overriding procedure Open (Object: in out Redland_World_Type_Without_Finalize);

   not overriding procedure Set_Rasqal (World: Redland_World_Type_Without_Finalize;
                                        Rasqal_World: Rasqal_World_Type_Without_Finalize'Class);

   not overriding function Get_Rasqal (World: Redland_World_Type_Without_Finalize)
                                       return Rasqal_World_Type_Without_Finalize;

   not overriding procedure Set_Raptor (World: Redland_World_Type_Without_Finalize;
                                        Raptor_World: Raptor_World_Type_Without_Finalize'Class);

   not overriding function Get_Raptor (World: Redland_World_Type_Without_Finalize)
                                       return Raptor_World_Type_Without_Finalize;

   not overriding procedure Set_Digest (World: Redland_World_Type_Without_Finalize; Name: String);

   not overriding function Get_Feature (World: Redland_World_Type_Without_Finalize;
                                        Feature: RDF.Redland.URI.URI_Type_Without_Finalize'Class)
                                        return RDF.Redland.Node.Node_Type;

   not overriding procedure Set_Feature (World: Redland_World_Type_Without_Finalize;
                                         Feature: RDF.Redland.URI.URI_Type_Without_Finalize'Class;
                                         Value: RDF.Redland.Node.Node_Type_Without_Finalize'Class);

   package Finalizer is new Redland_World_Handled_Record.Common_Handlers(Redland_World_Type_Without_Finalize);

   type Redland_World_Type is new Finalizer.Base_With_Finalization with null record;

   not overriding function Open return Redland_World_Type;

   -- librdf_world_set_error() and librdf_world_set_warning() deliberately not implemented.

   -- I deliberately not implement librdf_world_set_raptor_init_handler() and
   -- librdf_world_set_rasqal_init_handler().
   -- I recommend to use Set_Raptor and Set_Rasqal instead.

end RDF.Redland.World;
