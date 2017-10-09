with RDF.Auxiliary;
with RDF.Raptor.URI;
with RDF.Redland.World; use RDF.Redland.World;

package RDF.Redland.URI is

   subtype URI_String is RDF.Raptor.URI.URI_String;

   subtype URI_Handle is RDF.Raptor.URI.URI_Handle;

   type URI_Type_Without_Finalize is new RDF.Raptor.URI.URI_Handled_Record.Base_Object with null record;

   function To_Raptor (URI: URI_Type_Without_Finalize'Class) return RDF.Raptor.URI.URI_Type_Without_Finalize;

   not overriding function From_Raptor (URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class) return URI_Type_Without_Finalize;

   not overriding function As_String (URI: URI_Type_Without_Finalize) return String;

   not overriding procedure Print (URI: URI_Type_Without_Finalize; File: RDF.Auxiliary.C_File_Access);

   not overriding function Equals (Left, Right: URI_Type_Without_Finalize) return Boolean;

   overriding function "=" (Left, Right: URI_Type_Without_Finalize) return Boolean
                            renames Equals;

   not overriding function Compare(URI1, URI2: URI_Type_Without_Finalize) return RDF.Auxiliary.Comparison_Result;

   -- TODO: Create subtypes with dynamic predicate?
   not overriding function Is_File_URI (URI: URI_Type_Without_Finalize) return Boolean;

   not overriding function To_Filename (URI: URI_Type_Without_Finalize) return String;

   type URI_Type is new URI_Type_Without_Finalize with null record;

   overriding procedure Adjust(Object: in out URI_Type);

   overriding procedure Finalize_Handle(Object: URI_Type; Handle: URI_Handle);

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

   -- TODO: use a URI_Type subtype
   not overriding function From_Filename (World: Redland_World_Type_Without_Finalize'Class;
                                          Filename: String)
                                          return URI_Type;

   -- TODO: Stopped at librdf_uri_filter_func()

end RDF.Redland.URI;
