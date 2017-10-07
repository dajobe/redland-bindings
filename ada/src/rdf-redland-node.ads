with RDF.Redland.World; use RDF.Redland.World;
with RDF.Raptor.Term;

package RDF.Redland.Node is

   subtype Node_Handle is RDF.Raptor.Term.Term_Handle;

   type Node_Type_Without_Finalize is new RDF.Raptor.Term.Term_Type_Without_Finalize with null record;

   type Node_Type is new Node_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Object: Node_Type; Handle: Node_Handle);

   not overriding function Create (World: Redland_World_Type_Without_Finalize'Class) return Node_Type;

   -- "No identifier" is signified by empty string
   not overriding function From_Blank_Identifier (World: Redland_World_Type_Without_Finalize'Class;
                                                  ID: String)
                                                  return Node_Type;

   not overriding function From_URI_String (World: Redland_World_Type_Without_Finalize'Class;
                                            URI: String)
                                            return Node_Type;

   -- TODO: Stopped at librdf_new_node_from_literal()

end RDF.Redland.Node;
