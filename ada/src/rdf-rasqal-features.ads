with RDF.Rasqal.World; use RDF.Rasqal.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;

package RDF.Rasqal.Features is

   type Feature_Type is (Unknown, No_Net, Rand_Seed)
      with Convention=>C;
   for Feature_Type use (Unknown=>-1, No_Net=>0, Rand_Seed=>1);

   function Feature_From_URI (World: World_Type_Without_Finalize'Class; URI: URI_Type_Without_Finalize'Class) return Feature_Type;

end RDF.Rasqal.Features;
