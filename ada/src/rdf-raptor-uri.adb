with Ada.IO_Exceptions;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.Memory;
with RDF.Raptor.IOStream; use RDF.Raptor.IOStream;
with RDF.Raptor.Namespace_Stack; use RDF.Raptor.Namespace_Stack;
with RDF.Auxiliary.Convert; use RDF.Auxiliary.Convert;

package body RDF.Raptor.URI is

   function raptor_new_uri_from_counted_string (World_Handle: Raptor_World_Handle;
                                                  URI_String: char_array;
                                                  Length    : size_t)
                                                  return URI_Handle
     with Import, Convention=>C;

   function From_String(World: Raptor_World_Type_Without_Finalize'Class; Arg: URI_String) return URI_Type is
   begin
      -- LD_LIBRARY_PATH="" ltrace -n4 -llibraptor2.so.0 ./obj/test/debug/run_all_tests 2>&1| egrep ^[a-z]
      return From_Non_Null_Handle (raptor_new_uri_from_counted_string (Get_Handle (World), My_To_C_Without_Nul(String(Arg)), Arg'Length));
   end;

   function raptor_new_uri_from_uri_local_name (World_Handle: Raptor_World_Handle;
                                                  URI: URI_Handle;
                                                  Local_Name: char_array)
                                                  return URI_Handle
     with Import, Convention=>C;

   function From_URI_With_Local_Name(World: Raptor_World_Type_Without_Finalize'Class; URI: URI_Type_Without_Finalize'Class; Local_Name: String) return URI_Type is
   begin
      return From_Non_Null_Handle (raptor_new_uri_from_uri_local_name (Get_Handle (World), Get_Handle (URI), To_C (Local_Name)));
   end;

   function raptor_new_uri_from_uri_or_file_string(World_Handle: Raptor_World_Handle;
                                                     Base_URI: URI_Handle;
                                                     uri_or_file_string: char_array)
                                                     return URI_Handle
     with Import, Convention=>C;

   function From_URI_Or_File_String(World   : Raptor_World_Type_Without_Finalize'Class;
                                    Base_URI: URI_Type_Without_Finalize'Class;
                                    Uri_Or_File: String)
                                    return URI_Type is
   begin
      return From_Non_Null_Handle (raptor_new_uri_from_uri_or_file_string (Get_Handle (World), Get_Handle (Base_URI), To_C (Uri_Or_File)));
   end;

   function raptor_new_uri_relative_to_base_counted(World_Handle: Raptor_World_Handle;
                                                      Base_URI: URI_Handle;
                                                      uri_string: char_array;
                                                      uri_len: size_t)
                                                     return URI_Handle
     with Import, Convention=>C;

   function From_URI_Relative_To_Base(World: Raptor_World_Type_Without_Finalize'Class;
                                      Base_URI: URI_Type_Without_Finalize'Class;
                                      URI: URI_String)
                                      return URI_Type is
   begin
      return From_Non_Null_Handle (raptor_new_uri_relative_to_base_counted(Get_Handle (World),
                                                                             Get_Handle (Base_URI),
                                                                             My_To_C_Without_Nul(String(URI)),
                                                                             URI'Length));
   end;

   function raptor_new_uri_from_id(World_Handle: Raptor_World_Handle;
                                     Base_URI: URI_Handle;
                                     ID: char_array)
                                     return URI_Handle
     with Import, Convention=>C;

   function From_ID(World: Raptor_World_Type_Without_Finalize'Class; Base_URI: URI_Type_Without_Finalize'Class; ID: String) return URI_Type is
   begin
      return From_Non_Null_Handle (raptor_new_uri_from_id (Get_Handle (World), Get_Handle (Base_URI), To_C (ID)));
   end;

   function raptor_new_uri_for_rdf_concept (World_Handle: Raptor_World_Handle;
                                              Name: char_array)
                                              return URI_Handle
     with Import, Convention=>C;

   function From_RDF_Concept(World: Raptor_World_Type_Without_Finalize'Class; Name: String) return URI_Type is
   begin
      return From_Non_Null_Handle (raptor_new_uri_for_rdf_concept (Get_Handle (World), To_C (Name)));
   end;

   function raptor_new_uri_for_xmlbase (Old_URI: URI_Handle)
                                          return URI_Handle
     with Import, Convention=>C;

   function For_XML_Base(Old_URI: URI_Type_Without_Finalize'Class) return URI_Type is
   begin
      return From_Non_Null_Handle (raptor_new_uri_for_xmlbase (Get_Handle(Old_URI)));
   end;

   function raptor_new_uri_for_retrieval (Old_URI: URI_Handle)
                                          return URI_Handle
     with Import, Convention=>C;

   function For_Retrieval(Old_URI: URI_Type_Without_Finalize'Class) return URI_Type is
   begin
      return From_Non_Null_Handle (raptor_new_uri_for_retrieval (Get_Handle(Old_URI)));
   end;

   function raptor_uri_compare (URI1, URI2: URI_Handle) return int
     with Import, Convention=>C;

   function Compare(URI1, URI2: URI_Type_Without_Finalize) return RDF.Auxiliary.Comparison_Result is
   begin
      return RDF.Auxiliary.Comparison_Result (raptor_uri_compare (Get_Handle(URI1), Get_Handle(URI2)));
   end;

   function raptor_uri_equals (URI1, URI2: URI_Handle) return int
     with Import, Convention=>C;

   function Equals(URI1, URI2: URI_Type_Without_Finalize) return Boolean is
   begin
      return raptor_uri_equals (Get_Handle(URI1), Get_Handle(URI2)) /= 0;
   end;

   function raptor_uri_as_string(URI: URI_Handle)
                                   return chars_ptr
     with Import, Convention=>C;

   -- raptor_uri_as_counted_string() or raptor_uri_to_counted_string() is faster.
   -- But Ad2012 does not provide constructing strings of given length.
   function To_String(URI: URI_Type_Without_Finalize) return URI_String is
   begin
      -- raptor_uri_as_string() returns a pointer which exists as long as the URI object. No need to free it.
      return URI_String(String'(Value(raptor_uri_as_string(Get_Handle(URI)))));
   end;

   function raptor_uri_to_relative_uri_string (Base_URI, Reference_URI: URI_Handle) return chars_ptr
      with Import, Convention=>C;

   function To_Relative_URI_String(Base_URI, Reference_URI: URI_Type_Without_Finalize) return URI_String is
      C_Str: constant chars_ptr := raptor_uri_to_relative_uri_string (Get_Handle (Base_URI),Get_Handle (Reference_URI));
      Result: constant String := Value (C_Str);
   begin
      RDF.Raptor.Memory.raptor_free_memory (C_Str);
      return URI_String(Result);
   end;

   function raptor_uri_resolve_uri_reference(Base_URI, Reference_URI: char_array; buffer: char_array; length: size_t) return size_t
      with Import, Convention=>C;

   function Resolve_URI_Reference (Base_URI, Reference_URI: URI_String) return URI_String is
      Buffer_Length: constant size_t := Base_URI'Length + Reference_URI'Length + 1;
      Buffer       : char_array (1..Buffer_Length) := (others=>NUL);
      Dummy        : constant size_t := raptor_uri_resolve_uri_reference (To_C (String(Base_URI)), To_C (String(Reference_URI)), Buffer, Buffer_Length);
   begin
      return URI_String(To_Ada(Buffer));
   end;

   function raptor_uri_counted_filename_to_uri_string(filename: char_array; filename_len: size_t) return chars_ptr
      with Import, Convention=>C;

   function Filename_To_URI_String (Filename: String) return URI_String is
      Result1: constant chars_ptr := raptor_uri_counted_filename_to_uri_string (To_C (Filename, Append_Nul=>False), Filename'Length);
      Result: constant String := Value (Result1);
   begin
      RDF.Raptor.Memory.raptor_free_memory (Result1);
      return URI_String(Result);
   end;

   function raptor_uri_uri_string_is_absolute (URI_String: char_array) return int
      with Import, Convention=>C;

   function URI_String_Is_Absolute (Str: URI_String) return Boolean is
   begin
      return raptor_uri_uri_string_is_absolute(To_C(String(Str))) > 0; -- C sources show that <0 cannot be returned
   end;

   function raptor_uri_uri_string_is_file_uri (URI_String: char_array) return int
      with Import, Convention=>C;

   function URI_String_Is_File_URI (Str: URI_String) return Boolean is
   begin
      return raptor_uri_uri_string_is_file_uri(To_C(String(Str))) /= 0;
   end;

   function raptor_uri_uri_string_to_filename (URI_String: char_array) return chars_ptr
      with Import, Convention=>C;

   function URI_String_To_Filename (Str: URI_String) return String is
      Result1: constant chars_ptr := raptor_uri_uri_string_to_filename(To_C(String(Str)));
      Result: constant String := Value (Result1);
   begin
      RDF.Raptor.Memory.raptor_free_memory (Result1);
      return Result;
   end;

   function raptor_uri_uri_string_to_filename_fragment (URI_String: char_array; Fragment_P: access chars_ptr) return chars_ptr
      with Import, Convention=>C;

   function URI_String_To_Filename_And_Fragment(Str: URI_String) return Filename_And_Fragment is
      C_Filename : chars_ptr;
      C_Fragment : aliased chars_ptr;
   begin
      C_Filename := raptor_uri_uri_string_to_filename_fragment (To_C(String(Str)), C_Fragment'Unchecked_Access);
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

   function raptor_uri_print (URI: URI_Handle; File: RDF.Auxiliary.C_File_Access) return int
      with Import, Convention=>C;

   procedure Print (URI: URI_Type_Without_Finalize; File: RDF.Auxiliary.C_File_Access) is
   begin
      if raptor_uri_print (Get_Handle (URI), File) /= 0 then
         raise IOStream_Exception;
      end if;
   end;

   function raptor_uri_get_world (URI: URI_Handle) return Raptor_World_Handle
      with Import, Convention=>C;

   function Get_World (URI: URI_Type_Without_Finalize) return Raptor_World_Type_Without_Finalize is
   begin
      return From_Handle (raptor_uri_get_world (Get_Handle (URI)));
   end;

   function raptor_uri_write (URI: URI_Handle; Stream: IOStream_Handle) return int
      with Import, Convention=>C;

   procedure Write (URI: URI_Type_Without_Finalize; Stream: Base_Stream_Type'Class) is
      use all type Base_Stream_Type;
   begin
      if raptor_uri_write (Get_Handle(URI), Get_Handle(Stream)) /= 0 then
         raise IOStream_Exception;
      end if;
   end;

   function raptor_uri_file_exists (URI: URI_Handle) return int
      with Import, Convention=>C;

   function URI_File_Exists (URI: URI_Type_Without_Finalize) return Boolean is
      Result: constant int := raptor_uri_file_exists(Get_Handle(URI));
   begin
      if Result < 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Result /= 0;
   end;

   function raptor_uri_filename_exists (Filename: char_array) return int
      with Import, Convention=>C;

   function Filename_Exists (Filename: String) return Boolean is
      Result: constant int := raptor_uri_filename_exists(To_C(Filename, Append_Nul=>True));
   begin
      if Result < 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Result /= 0;
   end;

   procedure raptor_free_uri (Handle: URI_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle(Object: URI_Type; Handle: URI_Handle) is
   begin
      raptor_free_uri (Handle);
   end;

   function raptor_uri_copy (Handle: URI_Handle)
                               return URI_Handle
     with Import, Convention=>C;

   procedure Adjust(Object: in out URI_Type) is
      use RDF.Auxiliary;
   begin
      if Get_Handle(Object) /= null then
         Set_Handle_Hack(Object, Raptor_Uri_Copy (Get_Handle(Object)));
      end if;
   end;

   function Copy (Object: URI_Type_Without_Finalize'Class) return URI_Type_Without_Finalize is
   begin
      return From_Handle (raptor_uri_copy (Get_Handle(Object)) );
   end;

   function Get_Filename (Pair: Filename_And_Fragment) return String is (Pair.Filename);
   function Get_Fragment (Pair: Filename_And_Fragment) return String is (Pair.Fragment);

   function raptor_uri_to_turtle_string(Handle: Raptor_World_Handle;
                                          URI: URI_Handle;
                                          Stack: Namespace_Stack_Handle;
                                          Base_URI: URI_Handle)
                                          return Chars_Ptr
     with Import, Convention=>C;

   function To_Turtle_String (World: Raptor_World_Type_Without_Finalize'Class;
                              URI: URI_Type_Without_Finalize;
                              Stack: Namespace_Stack_Type_Without_Finalize'Class;
                              Base_URI: URI_Type_Without_Finalize)
                              return String is
      C_Str: constant chars_ptr := raptor_uri_to_turtle_string(Get_Handle(World),
                                                                 Get_Handle(URI),
                                                                 Get_Handle(Stack),
                                                                 Get_Handle(Base_URI));
   begin
      if C_Str = Null_Ptr then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      declare
         Result: constant String := Value(C_Str);
      begin
         RDF.Raptor.Memory.Raptor_Free_Memory(C_Str);
         return Result;
      end;
   end;

   function raptor_uri_turtle_write(Handle: Raptor_World_Handle;
                                      Stream: IOStream_Handle;
                                      URI: URI_Handle;
                                      Stack: Namespace_Stack_Handle;
                                      Base_URI: URI_Handle)
                                      return Int
     with Import, Convention=>C;

   procedure Turtle_Write (World: Raptor_World_Type_Without_Finalize'Class;
                           Stream: Base_Stream_Type'Class;
                           URI: URI_Type_Without_Finalize;
                           Stack: Namespace_Stack_Type_Without_Finalize'Class;
                           Base_URI: URI_Type_Without_Finalize) is
   begin
      if raptor_uri_turtle_write(Get_Handle(World),
                                   Get_Handle(Stream),
                                   Get_Handle(URI),
                                   Get_Handle(Stack),
                                   Get_Handle(Base_URI)) /= 0
      then
         raise IOStream_Exception;
      end if;
   end;

end RDF.Raptor.URI;
