with Interfaces.C; use Interfaces.C;

package body RDF.Rasqal.Features is

   function rasqal_feature_from_uri (World: RDF.Rasqal.World.Handle_Type; URI: RDF.Raptor.URI.Handle_Type) return Feature_Type
      with Import, Convention=>C;

   function Feature_From_URI (World: World_Type_Without_Finalize'Class; URI: URI_Type_Without_Finalize'Class) return Feature_Type is
      Value: constant Feature_Type := rasqal_feature_from_uri(Get_Handle(World), Get_Handle(URI));
   begin
      return (if Value'Valid then Value else Unknown);
   end;

   function rasqal_feature_value_type (Feature: Feature_Type) return int
      with Import, Convention=>C;

   function Get_Type (Feature: Feature_Type) return Feature_Value_Type is
      Value_Type: constant int := rasqal_feature_value_type(Feature);
   begin
      return (if Value_Type in 0..1 then Feature_Value_Type'Val(Value_Type) else Other);
   end;

end RDF.Rasqal.Features;
