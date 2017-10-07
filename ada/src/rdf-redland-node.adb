package body RDF.Redland.Node is

   procedure librdf_free_node (Node: Node_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Node_Type; Handle: Node_Handle) is
   begin
      librdf_free_node(Handle);
   end;

end RDF.Redland.Node;
