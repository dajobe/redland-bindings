with RDF.Auxiliary;
with RDF.Raptor.URI;
with RDF.Redland.World; use RDF.Redland.World;

package RDF.Redland.URI is

   subtype URI_String is RDF.Raptor.URI.URI_String;

   subtype URI_Handle is RDF.Raptor.URI.URI_Handle;

   type URI_Type_Without_Finalize is new RDF.Raptor.URI.URI_Handled_Record.Base_Object with null record;

   overriding function Adjust_Handle (Object: URI_Type_Without_Finalize; Handle: URI_Handle) return URI_Handle;

   overriding procedure Finalize_Handle (Object: URI_Type_Without_Finalize; Handle: URI_Handle);

   function To_Raptor (URI: URI_Type_Without_Finalize'Class) return RDF.Raptor.URI.URI_Type_Without_Finalize;

   not overriding function From_Raptor (URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class) return URI_Type_Without_Finalize;

   not overriding function As_String (URI: URI_Type_Without_Finalize) return String;

   not overriding procedure Print (URI: URI_Type_Without_Finalize; File: RDF.Auxiliary.C_File_Access);

   not overriding function Equals (Left, Right: URI_Type_Without_Finalize) return Boolean;

   overriding function "=" (Left, Right: URI_Type_Without_Finalize) return Boolean
                            renames Equals;

   not overriding function Compare(URI1, URI2: URI_Type_Without_Finalize) return RDF.Auxiliary.Comparison_Result;

   not overriding function Is_File_URI (URI: URI_Type_Without_Finalize) return Boolean;

   not overriding function To_Filename (URI: URI_Type_Without_Finalize) return String;

   -- From http://librdf.org/docs/api/redland-concepts.html:
   not overriding function Concept_Ms_Namespace (World: Redland_World_Type_Without_Finalize'Class)
                                                 return URI_Type_Without_Finalize;
   not overriding function Concept_Schema_Namespace (World: Redland_World_Type_Without_Finalize'Class)
                                                     return URI_Type_Without_Finalize;

   package Handlers is new RDF.Raptor.URI.URI_Handled_Record.Common_Handlers(URI_Type_Without_Finalize);

   type URI_Type is new Handlers.Base_With_Finalization with null record;

   type URI_Type_User is new Handlers.User_Type with null record;

   not overriding function From_String (World: Redland_World_Type_Without_Finalize'Class; URI: URI_String)
                                        return URI_Type;

   not overriding function From_URI_Local_Name (Old_URI: URI_Type_Without_Finalize'Class;
                                                Local_Name: String)
                                                return URI_Type;

   not overriding function Normalised_To_Base (URI_Str: URI_String;
                                               Source_URI, Base_URI: URI_Type_Without_Finalize'Class)
                                               return URI_Type;

   not overriding function Relative_To_Base (Base_URI: URI_Type_Without_Finalize'Class; Str: URI_String)
                                             return URI_Type;

   not overriding function From_Filename (World: Redland_World_Type_Without_Finalize'Class;
                                          Filename: String)
                                          return URI_Type;

end RDF.Redland.URI;
