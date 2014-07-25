with RDF.Auxilary.Simple_Limited_Handled_Record;
with RDF.Raptor.World; use RDF.Raptor.World;

package RDF.Raptor.URI is

   type URI_Type is new RDF.Auxilary.Simple_Limited_Handled_Record.Base_Object with null record;

   -- TODO:
   -- type URI_String is new String;

   -- null
   --overriding function Default_Handle(Object: URI_Type) return RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type;

   overriding procedure Finalize_Handle(Object: URI_Type; Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type);

   not overriding function From_String(World: World_Type; Arg: String) return URI_Type;

   not overriding function From_URI_With_Local_Name(World: World_Type; URI: URI_Type; Local_Name: String) return URI_Type;

   not overriding function From_URI_Or_File_String(World: World_Type;
                                                   Base_URI: URI_Type;
                                                   uri_or_file: String)
                                                   return URI_Type;

   not overriding function From_URI_Relative_To_Base(World: World_Type;
                          		             Base_URI: URI_Type;
                                                     URI_String: String)
                                                     return URI_Type;

   not overriding function From_ID(World: World_Type; Base_URI: URI_Type; ID: String) return URI_Type;

   not overriding function From_RDF_Concept(World: World_Type; Name: String) return URI_Type;

   not overriding function For_XML_Base(Old_URI: URI_Type) return URI_Type;

   not overriding function For_Retrieval(Old_URI: URI_Type) return URI_Type;

   not overriding function Compare(URI1, URI2: URI_Type) return RDF.Auxilary.Comparison_Result;

   not overriding function Equals(URI1, URI2: URI_Type) return Boolean;

   function "="(URI1, URI2: URI_Type) return Boolean renames Equals;

end RDF.Raptor.URI;
