with RDF.Raptor.Term;

package RDF.Redland.Node is

   subtype Node_Handle is RDF.Raptor.Term.Term_Handle;

   type Node_Type_Without_Finalize is new RDF.Raptor.Term.Term_Type_Without_Finalize with null record;

   type Node_Type is new Node_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Object: Node_Type; Handle: Node_Handle);

   -- TODO: Stopped at librdf_new_node()

end RDF.Redland.Node;
