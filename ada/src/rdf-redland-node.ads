with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.URI; use RDF.Redland.URI;
with RDF.Auxiliary;
with RDF.Raptor.Term; use RDF.Raptor.Term;
with RDF.Raptor.IOStream; use RDF.Raptor.IOStream;

package RDF.Redland.Node is

   -- TODO: Add "Pre" aspects for getters

   subtype Node_Kind is RDF.Raptor.Term.Term_Kind;

   subtype Node_Handle is RDF.Raptor.Term.Term_Handle;

   type Node_Type_Without_Finalize is new RDF.Raptor.Term.Term_Handled_Record.Base_Object with null record;

   function To_Raptor (Node: Node_Type_Without_Finalize'Class) return Term_Type_Without_Finalize;

   not overriding function From_Raptor (Term: Term_Type_Without_Finalize'Class) return Node_Type_Without_Finalize;

   not overriding function Encode (Node: Node_Type_Without_Finalize) return String;

   not overriding function Equals (Left, Right: Node_Type_Without_Finalize) return Boolean;

   overriding function "=" (Left, Right: Node_Type_Without_Finalize) return Boolean
                            renames Equals;

   not overriding function Get_Blank_Identifier (Node: Node_Type_Without_Finalize) return String;

   not overriding function Get_Li_Ordinal (Node: Node_Type_Without_Finalize) return Positive;

   not overriding function As_String (Node: Node_Type_Without_Finalize) return String;

   not overriding function As_Latin1 (Node: Node_Type_Without_Finalize) return String;

   not overriding function Get_Datatype_URI (Node: Node_Type_Without_Finalize) return URI_Type_Without_Finalize;

   not overriding function Is_WF_XML (Node: Node_Type_Without_Finalize) return Boolean;

   -- Return "" if no language
   not overriding function Get_Language (Node: Node_Type_Without_Finalize) return String;

   not overriding function Get_Type (Node: Node_Type_Without_Finalize) return Node_Kind;

   not overriding function Get_URI (Node: Node_Type_Without_Finalize) return URI_Type_Without_Finalize;

   not overriding function Is_Blank    (Node: Node_Type_Without_Finalize) return Boolean;
   not overriding function Is_Literal  (Node: Node_Type_Without_Finalize) return Boolean;
   not overriding function Is_Resource (Node: Node_Type_Without_Finalize) return Boolean;

   -- librdf_node_write() deliberately not bound

   not overriding procedure Print (Node: Node_Type_Without_Finalize; File: RDF.Auxiliary.C_File_Access);

   not overriding procedure Write (Node: Node_Type_Without_Finalize; Stream: Base_IOStream_Type'Class);

   package Finalizer is new With_Finalization(Node_Type_Without_Finalize);

   type Node_Type is new Finalizer.Derived with null record;

   overriding procedure Finalize_Handle (Object: Node_Type; Handle: Node_Handle);

   overriding function Adjust_Handle (Object: Node_Type; Handle: Node_Handle) return Node_Handle;

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

   not overriding function Decode (World: Redland_World_Type_Without_Finalize'Class;
                                   Buffer: String)
                                   return Node_Type;

   subtype Blank_Node_Type_Without_Finalize is Node_Type_Without_Finalize
     with Dynamic_Predicate => Is_Blank(Blank_Node_Type_Without_Finalize);
   subtype Literal_Node_Type_Without_Finalize is Node_Type_Without_Finalize
     with Dynamic_Predicate => Is_Literal(Literal_Node_Type_Without_Finalize);
   subtype Resource_Node_Type_Without_Finalize is Node_Type_Without_Finalize
     with Dynamic_Predicate => Is_Resource(Resource_Node_Type_Without_Finalize);

   subtype Blank_Node_Type is Node_Type
     with Dynamic_Predicate => Is_Blank(Blank_Node_Type);
   subtype Literal_Node_Type is Node_Type
     with Dynamic_Predicate => Is_Literal(Literal_Node_Type);
   subtype Resource_Node_Type is Node_Type
     with Dynamic_Predicate => Is_Resource(Resource_Node_Type);

end RDF.Redland.Node;
