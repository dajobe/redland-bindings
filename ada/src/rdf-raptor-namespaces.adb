with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary; use RDF.Auxiliary;
with RDF.Auxiliary.C_String_Holders; use RDF.Auxiliary.C_String_Holders;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Namespaces_Stacks; use RDF.Raptor.Namespaces_Stacks;
with RDF.Raptor.Memory;

package body RDF.Raptor.Namespaces is

   function C_Raptor_New_Namespace (Stack: RDF.Raptor.Namespaces_Stacks.Namespace_Stack_Handle_Type;
                                    Prefix: Char_Array;
                                    NS: Char_Array;
                                    Depth: Int)
                                    return Namespace_Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_namespace";

   function New_Namespace (Stack: RDF.Raptor.Namespaces_Stacks.Namespace_Stack_Type_Without_Finalize'Class;
                           Prefix: String;
                           NS: String;
                           Depth: Natural)
                           return Namespace_Type is
   begin
      return From_Non_Null_Handle( C_Raptor_New_Namespace(Get_Handle(Stack), To_C(Prefix, Append_Nul=>True), To_C(NS, Append_Nul=>True), Int(Depth)) );
   end;

   function C_Raptor_New_Namespace_From_URI (Stack: RDF.Raptor.Namespaces_Stacks.Namespace_Stack_Handle_Type;
                                             Prefix: char_array;
                                             URI: RDF.Raptor.URI.Handle_Type)
                                             return Namespace_Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_namespace_from_uri";

   function From_URI (Stack: RDF.Raptor.Namespaces_Stacks.Namespace_Stack_Type_Without_Finalize'Class;
                      Prefix: String;
                      URI: URI_Type_Without_Finalize'Class;
                      Depth: Integer)
                      return Namespace_Type is
   begin
      return From_Non_Null_Handle( C_Raptor_New_Namespace_From_URI (Get_Handle(Stack), To_C(Prefix, Append_Nul=>True), Get_Handle(URI)) );
   end;

   procedure C_Raptor_Free_Namespace (Handle: Namespace_Handle_Type)
     with Import, Convention=>C, External_Name=>"raptor_free_namespace";

   procedure Finalize_Handle (Object: Namespace_Type; Handle: Namespace_Handle_Type) is
   begin
      C_Raptor_Free_Namespace(Handle);
   end;

   function C_Raptor_Namespace_Get_Uri (Handle: Namespace_Handle_Type) return RDF.Raptor.URI.Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_namespace_get_uri";

   -- raptor_namespace_get_uri() may return NULL (for xmlns="")
   function Get_URI (NS: Namespace_Type_Without_Finalize) return URI_Type_Without_Finalize is
   begin
      return From_Handle( C_Raptor_Namespace_Get_Uri(Get_Handle(NS)) );
   end;

   function C_Raptor_Namespace_Get_Prefix (Handle: Namespace_Handle_Type) return chars_ptr
     with Import, Convention=>C, External_Name=>"raptor_namespace_get_prefix";

   function Get_Prefix (NS: Namespace_Type_Without_Finalize) return String is
   begin
      return Value( C_Raptor_Namespace_Get_Prefix(Get_Handle(NS)) );
   end;

   function C_Raptor_Namespace_Write (NS: Namespace_Handle_Type; Stream: RDF.Raptor.IOStream.Handle_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_namespace_write";

   procedure Write (NS: Namespace_Type_Without_Finalize; Stream: RDF.Raptor.IOStream.Stream_Type_Without_Finalize'Class) is
      use RDF.Raptor.IOStream;
   begin
      if C_Raptor_Namespace_Write(Get_Handle(NS), Get_Handle(Stream)) /= 0 then
         raise RDF.Raptor.IOStream.IOStream_Exception;
      end if;
   end;

   function C_Raptor_Namespace_Format_As_Xml (NS: Namespace_Handle_Type; Stream: access size_t) return chars_ptr
     with Import, Convention=>C, External_Name=>"raptor_namespace_format_as_xml";

   function Format_As_XML (NS: Namespace_Type_Without_Finalize) return String is
      C_Str: chars_ptr := C_Raptor_Namespace_Format_As_Xml(Get_Handle(NS), null);
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

   function C_Raptor_Xml_Namespace_String_Parse (Str: char_array; Prefix, URI: access chars_ptr) return int
     with Import, Convention=>C, External_Name=>"raptor_xml_namespace_string_parse";

   function String_Parse (NS: String) return Prefix_And_URI is
      C_Prefix, C_URI: aliased chars_ptr;
   begin
      if C_Raptor_Xml_Namespace_String_Parse(To_C(NS), C_Prefix'Access, C_URI'Access) /= 0 then
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
      if C_Raptor_Xml_Namespace_String_Parse(To_C(NS), C_Prefix'Access, null) /= 0 then
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
      if C_Raptor_Xml_Namespace_String_Parse(To_C(NS), null, C_URI'Access) /= 0 then
         raise RDF_Exception;
      end if;
      declare
         URI: constant URI_String := URI_String(String'(Value(C_URI)));
      begin
         RDF.Raptor.Memory.raptor_free_memory(C_URI);
         return URI;
      end;
   end;

end RDF.Raptor.Namespaces;
