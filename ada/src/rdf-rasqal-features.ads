with RDF.Rasqal.World; use RDF.Rasqal.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;

package RDF.Rasqal.Features is

   type Feature_Type is (Unknown, No_Net, Rand_Seed)
      with Convention=>C;
   for Feature_Type use (Unknown=>-1, No_Net=>0, Rand_Seed=>1);

   function Feature_From_URI (World: World_Type_Without_Finalize'Class; URI: URI_Type_Without_Finalize'Class) return Feature_Type;

   type Feature_Value_Type is (Other, Integer_Type, String_Type)
      with Convention=>C;
   for Feature_Value_Type use (Other=>-1, Integer_Type=>0, String_Type=>1);

   function Get_Type (Feature: Feature_Type) return Feature_Value_Type;

   -- TODO: stopped at rasqal_features_enumerate ()

end RDF.Rasqal.Features;
