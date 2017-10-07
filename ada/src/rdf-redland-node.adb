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

   function librdf_new_node_from_counted_uri_string (World: Redland_World_Handle;
                                                     Pointer: char_array;
                                                     Len: size_t)
                                                     return Node_Handle
     with Import, Convention=>C;

   function From_URI_String (World: Redland_World_Type_Without_Finalize'Class;
                             URI: String)
                             return Node_Type is
      Handle: constant Node_Handle :=
        librdf_new_node_from_counted_uri_string(Get_Handle(World), To_C(URI), URI'Length);
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_new_node_from_literal (World: Redland_World_Handle;
                                          Text, Language: char_array;
                                          Is_XML: int)
                                          return Node_Handle
     with Import, Convention=>C;

   function From_Literal (World: Redland_World_Type_Without_Finalize'Class;
                          Text: String;
                          Language: String;
                          Is_XML: Boolean := False)
                          return Node_Type is
      Handle: constant Node_Handle :=
        librdf_new_node_from_literal(Get_Handle(World),
                                     My_To_C_Without_Nul(Text),
                                     My_To_C_Without_Nul(Language),
                                     (if Is_XML then 1 else 0));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_new_node_from_node (Node: Node_Handle) return Node_Handle
     with Import, Convention=>C;

   procedure Adjust (Object: in out Node_Type) is
      use RDF.Raptor.Term;
   begin
      if Get_Handle(Object) /= null then
         Set_Handle_Hack(Object, librdf_new_node_from_node(Get_Handle(Object)));
      end if;
   end;

end RDF.Redland.Node;
