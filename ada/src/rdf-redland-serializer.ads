with RDF.Auxiliary.Limited_Handled_Record;

package RDF.Redland.Serializer is

   package Serializer_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Serializer_Type_Without_Finalize is new RDF.Redland.Serializer.Serializer_Handled_Record.Base_Object with null record;

   subtype Serializer_Handle is Serializer_Handled_Record.Access_Type;

   -- Stopped here

end RDF.Redland.Serializer;
