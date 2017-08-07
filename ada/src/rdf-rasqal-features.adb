package body RDF.Rasqal.Features is

   function rasqal_feature_from_uri (World: RDF.Rasqal.World.Handle_Type; URI: RDF.Raptor.URI.Handle_Type) return Feature_Type'Base
      with Import, Convention=>C;

   function Feature_From_URI (World: World_Type_Without_Finalize'Class; URI: URI_Type_Without_Finalize'Class) return Feature_Type is
      Value: Feature_Type'Base := rasqal_feature_from_uri(Get_Handle(World), Get_Handle(URI));
   begin
      if Feature_Type'Pos(Value) < 0 then
         Value := Unknown;
      end if;
      return Value;
   end;

end RDF.Rasqal.Features;
