with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.Convert; use RDF.Auxiliary.Convert;

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

--     function librdf_new_node_from_blank_identifier (World: Redland_World_Handle;
--                                                     Pointer: char_array_access)
--                                                     return Node_Handle
--       with Import, Convention=>C;

   function librdf_new_node_from_counted_blank_identifier (World: Redland_World_Handle;
                                                           Pointer: char_array_access;
                                                           Len: size_t)
                                                           return Node_Handle
     with Import, Convention=>C;

   function From_Blank_Identifier (World: Redland_World_Type_Without_Finalize'Class;
                                   ID: String)
                                   return Node_Type is
      ID2: aliased char_array := My_To_C_Without_Nul(ID);
      Pointer: constant char_array_access := (if ID = "" then null else ID2'Unchecked_Access);
      Handle: constant Node_Handle :=
        librdf_new_node_from_counted_blank_identifier(Get_Handle(World), Pointer, ID'Length);
   begin
      return From_Non_Null_Handle(Handle);
   end;

end RDF.Redland.Node;
