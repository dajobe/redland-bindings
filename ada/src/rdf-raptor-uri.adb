with Ada.IO_Exceptions;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Auxilary.Simple_Handled_Record;
with RDF.Auxilary.Simple_Limited_Handled_Record;
with RDF.Raptor.Memory;
--  use all type RDF.Auxilary.Simple_Handled_Record.Base_Object;

package body RDF.Raptor.URI is

   procedure C_Raptor_Free_URI (Handle: Handle_Type)
     with Import, Convention=>C, External_Name=>"raptor_free_uri";

   procedure Finalize_Handle(Object: URI_Type; Handle: Handle_Type) is
   begin
      C_Raptor_Free_URI (Handle);
   end;

   function C_Raptor_URI_Copy (Handle: Handle_Type)
                               return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_uri_copy";

   function Copy_Handle(Object: URI_Type; Handle: Handle_Type)
                        return Handle_Type is
   begin
      return C_Raptor_URI_Copy (Handle);
   end;

   function C_Raptor_New_Uri_From_Counted_String (World_Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type;
                                                  URI_String: char_array;
                                                  Length    : size_t)
                                                  return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_from_counted_string";

   function From_String(World: World_Type; Arg: String) return URI_Type is
   begin
      return From_Handle (C_Raptor_New_Uri_From_Counted_String (Get_Handle (World), To_C (Arg), Arg'Length));
   end;

   function C_Raptor_New_Uri_From_Uri_Local_Name (World_Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type;
                                                  URI: Handle_Type;
                                                  Local_Name: char_array)
                                                  return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_from_uri_local_name";

   function From_URI_With_Local_Name(World: World_Type; URI: URI_Type; Local_Name: String) return URI_Type is
   begin
      return From_Handle (C_Raptor_New_Uri_From_Uri_Local_Name (Get_Handle (World), Get_Handle (URI), To_C (Local_Name)));
   end;

   function C_Raptor_New_Uri_From_Uri_Or_File_String(World_Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type;
                                                     Base_URI: Handle_Type;
                                                     uri_or_file_string: char_array)
                                                     return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_from_uri_or_file_string";

   function From_URI_Or_File_String(World   : World_Type;
                                    Base_URI: URI_Type;
                                    Uri_Or_File: String)
                                    return URI_Type is
   begin
      return From_Handle (C_Raptor_New_Uri_From_Uri_Or_File_String (Get_Handle (World), Get_Handle (Base_URI), To_C (Uri_Or_File)));
   end;

   function C_Raptor_New_Uri_Relative_To_Base_Counted(World_Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type;
                                                      Base_URI: Handle_Type;
                                                      uri_string: char_array;
                                                      uri_len: size_t)
                                                     return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_relative_to_base_counted";

   function From_URI_Relative_To_Base(World: World_Type;
                          	      Base_URI: URI_Type;
                                      URI_String: String)
                                      return URI_Type is
   begin
      return From_Handle (C_Raptor_New_Uri_Relative_To_Base_Counted(Get_Handle (World),
       					                            Get_Handle (Base_URI),
                          					    To_C (URI_String),
                          					    URI_String'Length));
   end;

   function C_Raptor_New_Uri_From_ID(World_Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type;
                                     Base_URI: Handle_Type;
                                     ID: char_array)
                                     return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_from_id";

   function From_ID(World: World_Type; Base_URI: URI_Type; ID: String) return URI_Type is
   begin
      return From_Handle (C_Raptor_New_Uri_From_ID (Get_Handle (World), Get_Handle (Base_URI), To_C (ID)));
   end;

   function C_Raptor_New_Uri_For_Rdf_Concept (World_Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type;
                                              Name: char_array)
                                              return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_for_rdf_concept";

   function From_RDF_Concept(World: World_Type; Name: String) return URI_Type is
   begin
      return From_Handle (C_Raptor_New_Uri_For_Rdf_Concept (Get_Handle (World), To_C (Name)));
   end;

   function C_Raptor_New_Uri_For_Xmlbase (Old_URI: Handle_Type)
                                          return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_for_xmlbase";

   function For_XML_Base(Old_URI: URI_Type) return URI_Type is
   begin
      return From_Handle (C_Raptor_New_Uri_For_Xmlbase (Get_Handle(Old_URI)));
   end;

   function C_Raptor_New_Uri_For_Retrieval (Old_URI: Handle_Type)
                                          return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_for_retrieval";

   function For_Retrieval(Old_URI: URI_Type) return URI_Type is
   begin
      return From_Handle (C_Raptor_New_Uri_For_Retrieval (Get_Handle(Old_URI)));
   end;

   function C_Raptor_Uri_Compare (URI1, URI2: Handle_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_uri_compare";

   function Compare(URI1, URI2: URI_Type) return RDF.Auxilary.Comparison_Result is
   begin
      return RDF.Auxilary.Comparison_Result (C_Raptor_Uri_Compare (Get_Handle(URI1), Get_Handle(URI2)));
   end;

   function C_Raptor_Uri_Equals (URI1, URI2: Handle_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_uri_equals";

   function Equals(URI1, URI2: URI_Type) return Boolean is
   begin
      return C_Raptor_Uri_Equals (Get_Handle(URI1), Get_Handle(URI2)) /= 0;
   end;

   -- TODO: remove this function?
   function C_Raptor_Uri_As_String(URI: Handle_Type)
                                   return chars_ptr
     with Import, Convention=>C, External_Name=>"raptor_uri_as_string";

   -- TODO: raptor_uri_as_counted_string() is faster (or raptor_uri_to_counted_string)
   function To_String(URI: URI_Type) return String is
   begin
      -- raptor_uri_as_string() returns a pointer which exists as long as the URI object. No need to free it.
      return Value (C_Raptor_Uri_As_String (Get_Handle (URI)));
   end;

   function C_Raptor_Uri_To_Relative_Uri_String (Base_URI, Reference_URI: Handle_Type) return chars_ptr
      with Import, Convention=>C, External_Name=>"raptor_uri_to_relative_uri_string";

   function To_Relative_URI_String(Base_URI, Reference_URI: URI_Type) return String is
      C_Str: constant chars_ptr := C_Raptor_Uri_To_Relative_Uri_String (Get_Handle (Base_URI),Get_Handle (Reference_URI));
      Result: constant String := Value (C_Str);
   begin
      RDF.Raptor.Memory.raptor_free_memory (C_Str);
      return Result;
   end;

   function C_Raptor_Uri_Resolve_Uri_Reference(Base_URI, Reference_URI: char_array; buffer: char_array; length: size_t) return size_t
      with Import, Convention=>C, External_Name=>"raptor_uri_resolve_uri_reference";

   function Resolve_URI_Reference (Base_URI, Reference_URI: String) return String is
      Buffer_Length: constant size_t := Base_URI'Length + Reference_URI'Length + 1;
      Buffer       : char_array (1..Buffer_Length) := (others=>NUL);
      Dummy        : constant size_t := C_Raptor_Uri_Resolve_Uri_Reference (To_C (Base_URI), To_C (Reference_URI), Buffer, Buffer_Length);
   begin
      return To_Ada (Buffer);
   end;

   function C_Raptor_Uri_Counted_Filename_To_Uri_String(filename: char_array; filename_len: size_t) return chars_ptr
      with Import, Convention=>C, External_Name=>"raptor_uri_counted_filename_to_uri_string";

   function Filename_To_URI_String (Filename: String) return String is
      Result1: constant chars_ptr := C_Raptor_Uri_Counted_Filename_To_Uri_String (To_C (Filename), Filename'Length);
      Result: constant String := Value (Result1);
   begin
      RDF.Raptor.Memory.raptor_free_memory (Result1);
      return Result;
   end;

   function C_Raptor_Uri_Uri_String_Is_Absolute (URI_String: char_array) return int
      with Import, Convention=>C, External_Name=>"raptor_uri_uri_string_is_absolute";

   function URI_String_Is_Absolute (Str: String) return Boolean is
   begin
      return C_Raptor_Uri_Uri_String_Is_Absolute(To_C (Str)) > 0; -- C sources show that <0 cannot be returned
   end;

   function C_Raptor_Uri_Uri_String_Is_File_Uri (URI_String: char_array) return int
      with Import, Convention=>C, External_Name=>"raptor_uri_uri_string_is_file_uri";

   function URI_String_Is_File_URI (Str: String) return Boolean is
   begin
      return C_Raptor_Uri_Uri_String_Is_File_Uri(To_C (Str)) /= 0;
   end;

   function C_Raptor_Uri_Uri_String_To_Filename (URI_String: char_array) return chars_ptr
      with Import, Convention=>C, External_Name=>"raptor_uri_uri_string_to_filename";

   function URI_String_To_Filename (Str: String) return String is
      Result1: constant chars_ptr := C_Raptor_Uri_Uri_String_To_Filename (To_C (Str));
      Result: constant String := Value (Result1);
   begin
      RDF.Raptor.Memory.raptor_free_memory (Result1);
      return Result;
   end;

   type chars_ptr_ptr is access all chars_ptr with Convention=>C;

   function C_Raptor_Uri_Uri_String_To_Filename_Fragment (URI_String: char_array; Fragment_P: chars_ptr_ptr) return chars_ptr
      with Import, Convention=>C, External_Name=>"raptor_uri_uri_string_to_filename_fragment";

   function URI_String_To_Filename_And_Fragment(Str: String) return Filename_And_Fragment is
      C_Filename : chars_ptr;
      C_Fragment : aliased chars_ptr;
   begin
      C_Filename := C_Raptor_Uri_Uri_String_To_Filename_Fragment (To_C (Str), C_Fragment'Unchecked_Access);
      declare
         Filename: constant String := Value (C_Filename);
         Fragment: constant String := Value (C_Fragment);
         Result  : constant Filename_And_Fragment :=
            (Filename_Length => Filename'Length, Fragment_Length => Fragment'Length, Filename => Filename, Fragment => Fragment);
      begin
         RDF.Raptor.Memory.raptor_free_memory (C_Filename);
         if C_Fragment /= Null_Ptr then
            RDF.Raptor.Memory.raptor_free_memory (C_Fragment);
         end if;
         return Result;
      end;
   end;

   function C_Raptor_URI_Print (URI: Handle_Type; File: RDF.Auxilary.C_File_Access) return int
      with Import, Convention=>C, External_Name=>"raptor_uri_print";

   procedure Print (URI: URI_Type; File: RDF.Auxilary.C_File_Access) is
   begin
      if C_Raptor_URI_Print (Get_Handle (URI), File) /= 0 then
         raise Ada.IO_Exceptions.Use_Error;
      end if;
   end;

   function C_Raptor_URI_Get_World (URI: Handle_Type) return RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type
      with Import, Convention=>C, External_Name=>"raptor_uri_get_world";

   function Get_World (URI: URI_Type) return World_Type_Without_Finalize is
   begin
      return From_Handle (C_Raptor_URI_Get_World (Get_Handle (URI)));
   end;

   -- TODO: Finished with raptor_uri_write ()

end RDF.Raptor.URI;
