with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary; use RDF.Auxiliary;
with RDF.Auxiliary.C_String_Holders; use RDF.Auxiliary.C_String_Holders;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Namespace_Stack; use RDF.Raptor.Namespace_Stack;
with RDF.Raptor.Memory;

package body RDF.Raptor.Namespace is

   function raptor_new_namespace (Stack: RDF.Raptor.Namespace_Stack.Namespace_Stack_Handle_Type;
                                    Prefix: Char_Array;
                                    NS: Char_Array;
                                    Depth: Int)
                                    return Namespace_Handle_Type
     with Import, Convention=>C;

   function New_Namespace (Stack: RDF.Raptor.Namespace_Stack.Namespace_Stack_Type_Without_Finalize'Class;
                           Prefix: String;
                           NS: String;
                           Depth: Natural)
                           return Namespace_Type is
   begin
      return From_Non_Null_Handle( raptor_new_namespace(Get_Handle(Stack), To_C(Prefix, Append_Nul=>True), To_C(NS, Append_Nul=>True), Int(Depth)) );
   end;

   function raptor_new_namespace_from_uri (Stack: RDF.Raptor.Namespace_Stack.Namespace_Stack_Handle_Type;
                                             Prefix: char_array;
                                             URI: RDF.Raptor.URI.Handle_Type)
                                             return Namespace_Handle_Type
     with Import, Convention=>C;

   function From_URI (Stack: RDF.Raptor.Namespace_Stack.Namespace_Stack_Type_Without_Finalize'Class;
                      Prefix: String;
                      URI: URI_Type_Without_Finalize'Class;
                      Depth: Integer)
                      return Namespace_Type is
   begin
      return From_Non_Null_Handle( raptor_new_namespace_from_uri (Get_Handle(Stack), To_C(Prefix, Append_Nul=>True), Get_Handle(URI)) );
   end;

   procedure raptor_free_namespace (Handle: Namespace_Handle_Type)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Namespace_Type; Handle: Namespace_Handle_Type) is
   begin
      raptor_free_namespace(Handle);
   end;

   function raptor_namespace_get_uri (Handle: Namespace_Handle_Type) return RDF.Raptor.URI.Handle_Type
     with Import, Convention=>C;

   -- raptor_namespace_get_uri() may return NULL (for xmlns="")
   function Get_URI (NS: Namespace_Type_Without_Finalize) return URI_Type_Without_Finalize is
   begin
      return From_Handle( raptor_namespace_get_uri(Get_Handle(NS)) );
   end;

   function raptor_namespace_get_prefix (Handle: Namespace_Handle_Type) return chars_ptr
     with Import, Convention=>C;

   function Get_Prefix (NS: Namespace_Type_Without_Finalize) return String is
   begin
      return Value( raptor_namespace_get_prefix(Get_Handle(NS)) );
   end;

   function raptor_namespace_write (NS: Namespace_Handle_Type; Stream: RDF.Raptor.IOStream.Handle_Type) return int
     with Import, Convention=>C;

   procedure Write (NS: Namespace_Type_Without_Finalize; Stream: RDF.Raptor.IOStream.Base_Stream_Type'Class) is
      use RDF.Raptor.IOStream;
   begin
      if raptor_namespace_write(Get_Handle(NS), Get_Handle(Stream)) /= 0 then
         raise RDF.Raptor.IOStream.IOStream_Exception;
      end if;
   end;

   function raptor_namespace_format_as_xml (NS: Namespace_Handle_Type; Stream: access size_t) return chars_ptr
     with Import, Convention=>C;

   function Format_As_XML (NS: Namespace_Type_Without_Finalize) return String is
      C_Str: chars_ptr := raptor_namespace_format_as_xml(Get_Handle(NS), null);
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

   function Get_Prefix (Object: Prefix_And_URI) return String is (Object.Prefix);
   function Get_URI (Object: Prefix_And_URI) return URI_String is (Object.URI);

   function raptor_xml_namespace_string_parse (Str: char_array; Prefix, URI: access chars_ptr) return int
     with Import, Convention=>C;

   function String_Parse (NS: String) return Prefix_And_URI is
      C_Prefix, C_URI: aliased chars_ptr;
   begin
      if raptor_xml_namespace_string_parse(To_C(NS), C_Prefix'Access, C_URI'Access) /= 0 then
         raise RDF_Exception;
      end if;
      declare
         Prefix: constant String := Value(C_Prefix);
         URI: constant URI_String := URI_String(String'(Value(C_URI)));
      begin
         RDF.Raptor.Memory.raptor_free_memory(C_Prefix);
         RDF.Raptor.Memory.raptor_free_memory(C_URI   );
         return (Prefix_Length=>Prefix'Length, URI_Length=>URI'Length, Prefix=>Prefix, URI=>URI);
      end;
   end;

   function Extract_Prefix (NS: String) return String is
      C_Prefix: aliased chars_ptr;
   begin
      if raptor_xml_namespace_string_parse(To_C(NS), C_Prefix'Access, null) /= 0 then
         raise RDF_Exception;
      end if;
      declare
         Prefix: constant String := Value(C_Prefix);
      begin
         RDF.Raptor.Memory.raptor_free_memory(C_Prefix);
         return Prefix;
      end;
   end;

   function Extract_URI (NS: String) return URI_String is
      C_URI: aliased chars_ptr;
   begin
      if raptor_xml_namespace_string_parse(To_C(NS), null, C_URI'Access) /= 0 then
         raise RDF_Exception;
      end if;
      declare
         URI: constant URI_String := URI_String(String'(Value(C_URI)));
      begin
         RDF.Raptor.Memory.raptor_free_memory(C_URI);
         return URI;
      end;
   end;

end RDF.Raptor.Namespace;
