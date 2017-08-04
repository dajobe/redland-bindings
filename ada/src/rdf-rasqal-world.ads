with RDF.Auxiliary;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.Log;

package RDF.Rasqal.World is

   package Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type World_Type_Without_Finalize is new Handled_Record.Base_Object with null record;

   subtype Handle_Type is Handled_Record.Access_Type;

   overriding function Default_Handle(Object: World_Type_Without_Finalize) return Handle_Type;

   not overriding procedure Open(Object: World_Type_Without_Finalize);

   not overriding procedure Set_Log_Handler(World: World_Type_Without_Finalize; Handler: RDF.Raptor.Log.Log_Handler);

   type World_Type is new World_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Object: World_Type; Handle: Handle_Type);

   not overriding function Open return World_Type;

   type Warning_Level is range 0 .. 100;

   not overriding procedure Set_Warning_Level (World: World_Type; Level: Warning_Level);

   -- TODO: stopped at rasqal_world_get_raptor ()

end RDF.Rasqal.World;
