with RDF.Auxilary.Simple_Handled_Record;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.IOStream;

package RDF.Raptor.URI is

   subtype Handle_Type is RDF.Auxilary.Simple_Handled_Record.Access_Type;

   -- Only absolute URIs!
   type URI_Type_Without_Finalize is new RDF.Auxilary.Simple_Handled_Record.Base_Object with null record;

   overriding procedure Adjust(Object: in out URI_Type_Without_Finalize);

   not overriding function From_String(World: World_Type; Arg: String) return URI_Type_Without_Finalize;

   not overriding function From_URI_With_Local_Name(World: World_Type; URI: URI_Type_Without_Finalize; Local_Name: String) return URI_Type_Without_Finalize;

   not overriding function From_URI_Or_File_String(World: World_Type;
                                                   Base_URI: URI_Type_Without_Finalize;
                                                   uri_or_file: String)
                                                   return URI_Type_Without_Finalize;

   not overriding function From_URI_Relative_To_Base(World: World_Type;
                                                     Base_URI: URI_Type_Without_Finalize;
                                                     URI_String: String)
                                                     return URI_Type_Without_Finalize;

   not overriding function From_ID(World: World_Type; Base_URI: URI_Type_Without_Finalize; ID: String) return URI_Type_Without_Finalize;

   not overriding function From_RDF_Concept(World: World_Type; Name: String) return URI_Type_Without_Finalize;

   not overriding function For_XML_Base(Old_URI: URI_Type_Without_Finalize) return URI_Type_Without_Finalize;

   not overriding function For_Retrieval(Old_URI: URI_Type_Without_Finalize) return URI_Type_Without_Finalize;

   not overriding function Compare(URI1, URI2: URI_Type_Without_Finalize) return RDF.Auxilary.Comparison_Result;

   not overriding function Equals(URI1, URI2: URI_Type_Without_Finalize) return Boolean;

   overriding function "="(URI1, URI2: URI_Type_Without_Finalize) return Boolean renames Equals;

   not overriding function To_String(URI: URI_Type_Without_Finalize) return String;

   not overriding function To_Relative_URI_String(Base_URI, Reference_URI: URI_Type_Without_Finalize) return String;

   function Resolve_URI_Reference (Base_URI, Reference_URI: String) return String;

   function Filename_To_URI_String (Filename: String) return String;

   function URI_String_Is_Absolute (Str: String) return Boolean;

   function URI_String_Is_File_URI (Str: String) return Boolean;

   -- ignores the fragment
   function URI_String_To_Filename (Str: String) return String;

   type Filename_And_Fragment (Filename_Length, Fragment_Length: Natural) is
      record
         Filename: String(1..Filename_Length);
         Fragment: String(1..Fragment_Length);
      end record;

   function URI_String_To_Filename_And_Fragment(Str: String) return Filename_And_Fragment;

   -- Not supposed to be used, but included for completeness
   not overriding procedure Print (URI: URI_Type_Without_Finalize; File: RDF.Auxilary.C_File_Access);

   not overriding function Get_World (URI: URI_Type_Without_Finalize) return World_Type_Without_Finalize;

   not overriding procedure Write (URI: URI_Type_Without_Finalize; Stream: RDF.Raptor.IOStream.Base_Stream_Type'Class);

   not overriding function URI_File_Exists (URI: URI_Type_Without_Finalize) return Boolean;

   function Filename_Exists (Filename: String) return Boolean;

   -- TODO: raptor_uri_to_turtle_string (), raptor_uri_turtle_write ()

   type URI_Type is new URI_Type_Without_Finalize with null record;

   -- TODO:
   -- type URI_String is new String;

   -- null
   --overriding function Default_Handle(Object: URI_Type) return Handle_Type;

   overriding procedure Finalize_Handle(Object: URI_Type; Handle: Handle_Type);

end RDF.Raptor.URI;
