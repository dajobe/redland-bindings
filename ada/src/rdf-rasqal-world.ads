with RDF.Auxiliary;
with RDF.Auxiliary.Limited_Handled_Record;

package RDF.Rasqal.World is

   package Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type World_Type_Without_Finalize is new Handled_Record.Base_Object with null record;

   subtype Handle_Type is Handled_Record.Access_Type;

   type World_Type is new World_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Object: World_Type; Handle: Handle_Type);

   -- TODO: stopped at rasqal_new_world ()

end RDF.Rasqal.World;
