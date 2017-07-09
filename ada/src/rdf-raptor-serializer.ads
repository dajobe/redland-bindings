with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.Iostream;
with RDF.Raptor.URI; use RDF.Raptor.URI;

package RDF.Raptor.Serializer is

   package Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype Handle_Type is Handled_Record.Access_Type;

   type Serializer_Type_Without_Finalize is new Handled_Record.Base_Object with null record;

   -- WARNING: Other order of arguments than in C
   not overriding procedure Start_To_Iostream (Serializer: Serializer_Type_Without_Finalize;
                                               Iostream: RDF.Raptor.Iostream.Base_Stream_Type'Class;
                                               URI: URI_Type_Without_Finalize := From_Handle(null));

   not overriding procedure Start_To_Filename (Serializer: Serializer_Type_Without_Finalize; Filename: String);

   -- TODO: raptor_serializer_start_to_string()

   type Serializer_Type is new Serializer_Type_Without_Finalize with null record;

   not overriding function New_Serializer (World: World_Type) return Serializer_Type;
   not overriding function New_Serializer (World: World_Type; Syntax_Name: String) return Serializer_Type;

   overriding procedure Finalize_Handle (Serializer: Serializer_Type; Handle: Handle_Type);

end RDF.Raptor.Serializer;
