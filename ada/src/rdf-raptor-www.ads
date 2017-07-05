with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.World;

package RDF.Raptor.WWW is

   package WWW_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype WWW_Handle_Type is WWW_Handled_Record.Access_Type;

   type WWW_Type_Without_Finalize is new WWW_Handled_Record.Base_Object with null record;



   type WWW_Type is new WWW_Type_Without_Finalize with null record;

   function New_WWW (World: RDF.Raptor.World.World_Type'Class) return WWW_Type;

   -- TODO: Stopped at raptor_new_www_with_connection()

end RDF.Raptor.WWW;
