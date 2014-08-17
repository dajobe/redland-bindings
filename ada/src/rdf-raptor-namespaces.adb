with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxilary; use RDF.Auxilary;
with RDF.Auxilary.C_String_Holders; use RDF.Auxilary.C_String_Holders;
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

   -- FIXME: raptor_namespace_get_uri() may return NULL
   function Get_URI (NS: Namespace_Type_Without_Finalize) return URI_Type_Without_Finalize is
   begin
      return From_Non_Null_Handle( C_Raptor_Namespace_Get_Uri(Get_Handle(NS)) );
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
         raise RDF.Auxilary.RDF_Exception;
      end if;
      declare
         Result: constant String := Value(C_Str);
      begin
         RDF.Raptor.Memory.Raptor_Free_Memory(C_Str);
         return Result;
      end;
   end;

end RDF.Raptor.Namespaces;
