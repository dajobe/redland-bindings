package body RDF.Rasqal.Features is

   function rasqal_feature_from_uri (World: RDF.Rasqal.World.Handle_Type; URI: RDF.Raptor.URI.Handle_Type) return Feature_Type
      with Import, Convention=>C;

   function Feature_From_URI (World: World_Type_Without_Finalize'Class; URI: URI_Type_Without_Finalize'Class) return Feature_Type is
      Value: constant Feature_Type := rasqal_feature_from_uri(Get_Handle(World), Get_Handle(URI));
   begin
      return (if Value'Valid then Value else Unknown);
   end;

end RDF.Rasqal.Features;
