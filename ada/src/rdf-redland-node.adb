package body RDF.Redland.Node is

   procedure librdf_free_node (Node: Node_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Node_Type; Handle: Node_Handle) is
   begin
      librdf_free_node(Handle);
   end;

   function librdf_new_node (World: Redland_World_Handle) return Node_Handle
     with Import, Convention=>C;

   function Create (World: Redland_World_Type_Without_Finalize'Class) return Node_Type is
   begin
      return From_Non_Null_Handle(librdf_new_node(Get_Handle(World)));
   end;

end RDF.Redland.Node;
