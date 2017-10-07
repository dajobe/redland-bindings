with RDF.Redland.World; use RDF.Redland.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Term;

package RDF.Redland.Node is

   subtype Node_Handle is RDF.Raptor.Term.Term_Handle;

   type Node_Type_Without_Finalize is new RDF.Raptor.Term.Term_Type_Without_Finalize with null record;

   type Node_Type is new Node_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Object: Node_Type; Handle: Node_Handle);

   overriding procedure Adjust (Object: in out Node_Type);

   not overriding function Create (World: Redland_World_Type_Without_Finalize'Class) return Node_Type;

   -- "No identifier" is signified by empty string
   not overriding function From_Blank_Identifier (World: Redland_World_Type_Without_Finalize'Class;
                                                  ID: String)
                                                  return Node_Type;

   not overriding function From_URI_String (World: Redland_World_Type_Without_Finalize'Class;
                                            URI: URI_String)
                                            return Node_Type;

   not overriding function From_Literal (World: Redland_World_Type_Without_Finalize'Class;
                                         Text: String;
                                         Language: String;
                                         Is_XML: Boolean := False)
                                         return Node_Type;

   not overriding function From_Normalised_URI_String (World: Redland_World_Type_Without_Finalize'Class;
                                                       URI: URI_String;
                                                       Source_URI, Base_URI: URI_Type_Without_Finalize'Class)
                                                       return Node_Type;

   not overriding function From_Typed_Literal (World: Redland_World_Type_Without_Finalize'Class;                                               Text: String;
                                               Language: String;
                                               Datatype: URI_Type_Without_Finalize'Class)
                                               return Node_Type;

   not overriding function From_URI (World: Redland_World_Type_Without_Finalize'Class;
                                     URI: URI_Type_Without_Finalize'Class)
                                     return Node_Type;

   not overriding function From_URI_Local_Name (World: Redland_World_Type_Without_Finalize'Class;
                                                URI: URI_Type_Without_Finalize'Class;
                                                Local_Name: String)
                                                return Node_Type;

   -- TODO: Stopped at librdf_node_decode()

end RDF.Redland.Node;
