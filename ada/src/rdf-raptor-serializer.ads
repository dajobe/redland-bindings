with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.World; use RDF.Raptor.World;

package RDF.Raptor.Serializer is

   package Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype Handle_Type is Handled_Record.Access_Type;

   type Serializer_Type_Without_Finalize is new Handled_Record.Base_Object with null record;

   type Serializer_Type is new Serializer_Type_Without_Finalize with null record;

   not overriding function New_Serializer (World: World_Type) return Serializer_Type;
   not overriding function New_Serializer (World: World_Type; Syntax_Name: String) return Serializer_Type;

end RDF.Raptor.Serializer;
