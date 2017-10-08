with RDF.Raptor.URI;
with RDF.Redland.World; use RDF.Redland.World;

package RDF.Redland.URI is

   subtype URI_String is RDF.Raptor.URI.URI_String;

   subtype URI_Handle is RDF.Raptor.URI.URI_Handle;

   type URI_Type_Without_Finalize is new RDF.Raptor.URI.URI_Type_Without_Finalize with null record;

   type URI_Type is new URI_Type_Without_Finalize with null record;

   not overriding function From_String (World: Redland_World_Type_Without_Finalize'Class; URI: URI_String)
                                        return URI_Type;

   -- TODO: Stopped at librdf_new_uri_from_uri()

end RDF.Redland.URI;
