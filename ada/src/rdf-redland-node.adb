with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.Convert; use RDF.Auxiliary.Convert;
with RDF.Auxiliary.C_Pointers;

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
                             URI: URI_String)
                             return Node_Type is
      Handle: constant Node_Handle :=
        librdf_new_node_from_counted_uri_string(Get_Handle(World), To_C(String(URI)), URI'Length);
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

   function librdf_new_node_from_normalised_uri_string (World: Redland_World_Handle;
                                                        URI_String: char_array;
                                                        Source_URI, Base_URI: URI_Handle)
                                                        return Node_Handle
     with Import, Convention=>C;

   function From_Normalised_URI_String (World: Redland_World_Type_Without_Finalize'Class;
                                        URI: URI_String;
                                        Source_URI, Base_URI: URI_Type_Without_Finalize'Class)
                                        return Node_Type is
      Handle: constant Node_Handle :=
        librdf_new_node_from_normalised_uri_string(Get_Handle(World),
                                                   My_To_C_Without_Nul(String(URI)),
                                                   Get_Handle(Source_URI),
                                                   Get_Handle(Base_URI));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_new_node_from_typed_counted_literal (World: Redland_World_Handle;
                                                        Text: char_array;
                                                        Text_Len: size_t;
                                                        Language: char_array;
                                                        Language_Len: size_t;
                                                        Datatype: URI_Handle)
                                                        return Node_Handle
     with Import, Convention=>C;

   function From_Typed_Literal (World: Redland_World_Type_Without_Finalize'Class;
                                Text: String;
                                Language: String;
                                Datatype: URI_Type_Without_Finalize'Class)
                                return Node_Type is
      Handle: constant Node_Handle :=
        librdf_new_node_from_typed_counted_literal(Get_Handle(World),
                                                   My_To_C_Without_Nul(Text),
                                                   size_t(Text'Length),
                                                   My_To_C_Without_Nul(Language),
                                                   size_t(Language'Length),
                                                   Get_Handle(Datatype));

   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_new_node_from_uri (World: Redland_World_Handle; URI: URI_Handle)
                                      return Node_Handle
     with Import, Convention=>C;

   function From_URI (World: Redland_World_Type_Without_Finalize'Class;
                      URI: URI_Type_Without_Finalize'Class)
                      return Node_Type is
   begin
      return From_Non_Null_Handle(librdf_new_node_from_uri(Get_Handle(World), Get_Handle(URI)));
   end;

   function librdf_new_node_from_uri_local_name (World: Redland_World_Handle;
                                                 URI: URI_Handle;
                                                 Local_Name: char_array)
                                                 return Node_Handle
     with Import, Convention=>C;

   function From_URI_Local_Name (World: Redland_World_Type_Without_Finalize'Class;
                                 URI: URI_Type_Without_Finalize'Class;
                                 Local_Name: String)
                                 return Node_Type is
      Handle: constant Node_Handle :=
        librdf_new_node_from_uri_local_name(Get_Handle(World),
                                            Get_Handle(URI),
                                            My_To_C_Without_Nul(Local_Name));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_node_decode (World: Redland_World_Handle;
                                Size_P: chars_ptr;
                                Buffer: char_array;
                                Length: size_t)
                                return Node_Handle
     with Import, Convention=>C;

   function Decode (World: Redland_World_Type_Without_Finalize'Class;
                    Buffer: String)
                    return Node_Type is
      Handle: constant Node_Handle :=
        librdf_node_decode(Get_Handle(World), Null_Ptr, My_To_C_Without_Nul(Buffer), Buffer'Length);
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_node_encode (Node: Node_Handle; Buffer: char_array_access; Length: size_t) return size_t
     with Import, Convention=>C;

   function Encode (Node: Node_Type_Without_Finalize) return String is
      Length: constant size_t := librdf_node_encode(Get_Handle(Node), null, 0);
--        Buffer: aliased char_array(1..Length); -- GNAT 7.2.0 produces a warning
      Buffer: aliased char_array := (1..Length => NUL); --
      Length2: constant size_t := librdf_node_encode(Get_Handle(Node), Buffer'Unchecked_Access, Length);
      pragma Unreferenced(Length2);
   begin
      return To_Ada(Buffer, Trim_Nul=>False);
   end;

   function librdf_node_equals (Left, Right: Node_Handle) return int
     with Import, Convention=>C;

   function Equals (Left, Right: Node_Type_Without_Finalize) return Boolean is
   begin
      return librdf_node_equals(Get_Handle(Left), Get_Handle(Right)) /= 0;
   end;

   type Size_T_P is access all size_t with Convention=>C;

   function librdf_node_get_counted_blank_identifier (Node: Node_Handle; Length: Size_T_P)
                                                      return RDF.Auxiliary.C_Pointers.Pointer
     with Import, Convention=>C;

   function Get_Blank_Identifier (Node: Node_Type_Without_Finalize) return String is
      Length: aliased size_t;
      Buffer: constant RDF.Auxiliary.C_Pointers.Pointer :=
        librdf_node_get_counted_blank_identifier(Get_Handle(Node), Length'Unchecked_Access);
   begin
      return Value_With_Possible_NULs(Buffer, Length);
   end;

end RDF.Redland.Node;
