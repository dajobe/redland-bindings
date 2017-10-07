with RDF.Raptor.URI;

package RDF.Redland.URI is

   subtype URI_Handle is RDF.Raptor.URI.URI_Handle;

   type URI_Type_Without_Finalize is new RDF.Raptor.URI.URI_Type_Without_Finalize with null record;

   type URI_Type is new URI_Type_Without_Finalize with null record;

   -- TODO: Stopped at librdf_new_uri()

end RDF.Redland.URI;
