with RDF.Auxilary.Simple_Handled_Record;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.IOStream;

package RDF.Raptor.URI is

   -- Only absolute URIs!
   type URI_Type is new RDF.Auxilary.Simple_Handled_Record.Base_Object with null record;

   subtype Handle_Type is RDF.Auxilary.Simple_Handled_Record.Access_Type;

   -- TODO:
   -- type URI_String is new String;

   -- TODO: *_Without_Finalize

   -- null
   --overriding function Default_Handle(Object: URI_Type) return Handle_Type;

   overriding procedure Finalize_Handle(Object: URI_Type; Handle: Handle_Type);

   overriding function Copy_Handle(Object: URI_Type; Handle: Handle_Type)
                                   return Handle_Type;

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

   overriding function "="(URI1, URI2: URI_Type) return Boolean renames Equals;

   not overriding function To_String(URI: URI_Type) return String;

   not overriding function To_Relative_URI_String(Base_URI, Reference_URI: URI_Type) return String;

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
   not overriding procedure Print (URI: URI_Type; File: RDF.Auxilary.C_File_Access);

   not overriding function Get_World (URI: URI_Type) return World_Type_Without_Finalize;

   not overriding procedure Write (URI: URI_Type; Stream: RDF.Raptor.IOStream.Base_Stream_Type'Class);

   not overriding function URI_File_Exists (URI: URI_Type) return Boolean;

   function Filename_Exists (Filename: String) return Boolean;

   -- TODO: raptor_uri_to_turtle_string (), raptor_uri_turtle_write ()

end RDF.Raptor.URI;
