with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Auxilary.Simple_Handled_Record;
with RDF.Auxilary.Simple_Limited_Handled_Record;
--  use all type RDF.Auxilary.Simple_Handled_Record.Base_Object;

package body RDF.Raptor.URI is

   procedure C_Raptor_Free_URI (Handle: RDF.Auxilary.Simple_Handled_Record.Access_Type)
     with Import, Convention=>C, External_Name=>"raptor_free_uri";

   procedure Finalize_Handle(Object: URI_Type; Handle: RDF.Auxilary.Simple_Handled_Record.Access_Type) is
   begin
      C_Raptor_Free_URI (Handle);
   end;

   function C_Raptor_URI_Copy (Handle: RDF.Auxilary.Simple_Handled_Record.Access_Type)
                               return RDF.Auxilary.Simple_Handled_Record.Access_Type
     with Import, Convention=>C, External_Name=>"raptor_uri_copy";

   function Copy_Handle(Object: URI_Type; Handle: RDF.Auxilary.Simple_Handled_Record.Access_Type)
                        return RDF.Auxilary.Simple_Handled_Record.Access_Type is
   begin
      return C_Raptor_URI_Copy (Handle);
   end;

   function C_Raptor_New_Uri_From_Counted_String (World_Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type;
                                                  URI_String: char_array_access;
                                                  Length    : size_t)
                                                  return RDF.Auxilary.Simple_Handled_Record.Access_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_from_counted_string";

   function From_String(World: World_Type; Arg: String) return URI_Type is
      C_Str: aliased char_array := To_C (Arg, Append_Nul=>False);
   begin
      return From_Handle (C_Raptor_New_Uri_From_Counted_String (Get_Handle (World), C_Str'Unchecked_Access, Arg'Length));
   end;

   function C_Raptor_New_Uri_From_Uri_Local_Name (World_Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type;
                                                  URI: RDF.Auxilary.Simple_Handled_Record.Access_Type;
                                                  Local_Name: char_array_access)
                                                  return RDF.Auxilary.Simple_Handled_Record.Access_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_from_uri_local_name";

   function From_URI_With_Local_Name(World: World_Type; URI: URI_Type; Local_Name: String) return URI_Type is
      C_Local_Name_Str: aliased char_array := To_C (Local_Name, Append_Nul=>True);
   begin
      return From_Handle (C_Raptor_New_Uri_From_Uri_Local_Name (Get_Handle (World),
                                                                Get_Handle (URI),
								C_Local_Name_Str'Unchecked_Access));
   end;

   function C_Raptor_New_Uri_From_Uri_Or_File_String(World_Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type;
                                                     Base_URI: RDF.Auxilary.Simple_Handled_Record.Access_Type;
                                                     uri_or_file_string: char_array_access)
                                                     return RDF.Auxilary.Simple_Handled_Record.Access_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_from_uri_or_file_string";

   function From_URI_Or_File_String(World   : World_Type;
                                    Base_URI: URI_Type;
                                    Uri_Or_File: String)
                                    return URI_Type is
      C_Str: aliased char_array := To_C (Uri_Or_File, Append_Nul=>True);
   begin
      return From_Handle (C_Raptor_New_Uri_From_Uri_Or_File_String (Get_Handle (World), Get_Handle (Base_URI), C_Str'Unchecked_Access));
   end;

   function C_Raptor_New_Uri_Relative_To_Base_Counted(World_Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type;
                                                      Base_URI: RDF.Auxilary.Simple_Handled_Record.Access_Type;
                                                      uri_string: char_array_access;
                                                      uri_len: size_t)
                                                     return RDF.Auxilary.Simple_Handled_Record.Access_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_relative_to_base_counted";

   function From_URI_Relative_To_Base(World: World_Type;
                          	      Base_URI: URI_Type;
                                      URI_String: String)
                                      return URI_Type is
      C_Str: aliased char_array := To_C (URI_String, Append_Nul=>False);
   begin
      return From_Handle (C_Raptor_New_Uri_Relative_To_Base_Counted(Get_Handle (World),
                          					    Get_Handle (Base_URI),
                          					    C_Str'Unchecked_Access,
                          					    C_Str'Length));
   end;

   function C_Raptor_New_Uri_From_ID(World_Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type;
                                     Base_URI: RDF.Auxilary.Simple_Handled_Record.Access_Type;
                                     ID: char_array_access)
                                     return RDF.Auxilary.Simple_Handled_Record.Access_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_from_id";

   function From_ID(World: World_Type; Base_URI: URI_Type; ID: String) return URI_Type is
      C_Str: aliased char_array := To_C (ID, Append_Nul=>True);
   begin
      return From_Handle (C_Raptor_New_Uri_From_ID (Get_Handle (World), Get_Handle (Base_URI), C_Str'Unchecked_Access));
   end;

   function C_Raptor_New_Uri_For_Rdf_Concept (World_Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type;
                                              Name: char_array_access)
                                              return RDF.Auxilary.Simple_Handled_Record.Access_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_for_rdf_concept";

   function From_RDF_Concept(World: World_Type; Name: String) return URI_Type is
      C_Str: aliased char_array := To_C (Name, Append_Nul=>True);
   begin
      return From_Handle (C_Raptor_New_Uri_For_Rdf_Concept (Get_Handle (World), C_Str'Unchecked_Access));
   end;

   function C_Raptor_New_Uri_For_Xmlbase (Old_URI: RDF.Auxilary.Simple_Handled_Record.Access_Type)
                                          return RDF.Auxilary.Simple_Handled_Record.Access_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_for_xmlbase";

   function For_XML_Base(Old_URI: URI_Type) return URI_Type is
   begin
      return From_Handle (C_Raptor_New_Uri_For_Xmlbase (Get_Handle(Old_URI)));
   end;

   function C_Raptor_New_Uri_For_Retrieval (Old_URI: RDF.Auxilary.Simple_Handled_Record.Access_Type)
                                          return RDF.Auxilary.Simple_Handled_Record.Access_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_for_retrieval";

   function For_Retrieval(Old_URI: URI_Type) return URI_Type is
   begin
      return From_Handle (C_Raptor_New_Uri_For_Retrieval (Get_Handle(Old_URI)));
   end;

   function C_Raptor_Uri_Compare (URI1, URI2: RDF.Auxilary.Simple_Handled_Record.Access_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_uri_compare";

   function Compare(URI1, URI2: URI_Type) return RDF.Auxilary.Comparison_Result is
   begin
      return RDF.Auxilary.Comparison_Result (C_Raptor_Uri_Compare (Get_Handle(URI1), Get_Handle(URI2)));
   end;

   function C_Raptor_Uri_Equals (URI1, URI2: RDF.Auxilary.Simple_Handled_Record.Access_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_uri_equals";

   function Equals(URI1, URI2: URI_Type) return Boolean is
   begin
      return C_Raptor_Uri_Equals (Get_Handle(URI1), Get_Handle(URI2)) /= 0;
   end;

end RDF.Raptor.URI;
