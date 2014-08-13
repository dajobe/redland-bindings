with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxilary; use RDF.Auxilary;
with RDF.Auxilary.C_String_Holders; use RDF.Auxilary.C_String_Holders;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;

package body RDF.Raptor.Namespaces is

   function C_Raptor_New_Namespace_From_URI (Stack: Namespace_Stack_Handle_Type;
                                             Prefix: char_array;
                                             URI: RDF.Raptor.URI.Handle_Type)
                                             return Namespace_Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_namespace_from_uri";

   function From_URI (Stack: Namespace_Stack_Type_Without_Finalize'Class;
                      Prefix: String;
                      URI: URI_Type_Without_Finalize'Class;
                      Depth: Integer)
                      return Namespace_Type is
   begin
      return From_Non_Null_Handle( C_Raptor_New_Namespace_From_URI (Get_Handle(Stack), To_C(Prefix, Append_Nul=>True), Get_Handle(URI)) );
   end;

   function Defaults_Conversion is new Ada.Unchecked_Conversion(Source=>Defaults_Type, Target=>Interfaces.C.int);

   function C_Raptor_New_Namespaces (World: RDF.Raptor.World.Handle_Type;
                                     Defaults: int)
                                     return Namespace_Stack_Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_namespaces";

   function Create_Stack (World: RDF.Raptor.World.World_Type_Without_Finalize'Class; Defaults: Defaults_Type)
                          return Namespace_Stack_Type is
   begin
      return From_Non_Null_Handle( C_Raptor_New_Namespaces(Get_Handle(World), Defaults_Conversion(Defaults)) );
   end;

   procedure C_Raptor_Namespaces_Clear (Stack: Namespace_Stack_Handle_Type)
     with Import, Convention=>C, External_Name=>"raptor_namespaces_clear";

   procedure Clear (Stack: Namespace_Stack_Type_Without_Finalize) is
   begin
      C_Raptor_Namespaces_Clear(Get_Handle(Stack));
   end;

   procedure C_Raptor_Free_Namespaces (Stack: Namespace_Stack_Handle_Type)
     with Import, Convention=>C, External_Name=>"raptor_free_namespaces";

   procedure Finalize_Handle(Object: Namespace_Stack_Type; Handle: Namespace_Stack_Handle_Type) is
   begin
      C_Raptor_Free_Namespaces(Handle);
   end;

   procedure C_Raptor_Namespaces_Start_Namespace (Stack: Namespace_Stack_Handle_Type; Namespace: Namespace_Handle_Type)
     with Import, Convention=>C, External_Name=>"raptor_namespaces_start_namespace";

   procedure Start_Namespace (Stack: Namespace_Stack_Type_Without_Finalize; Namespace: Namespace_Type'Class) is
   begin
      C_Raptor_Namespaces_Start_Namespace(Get_Handle(Stack), Get_Handle(Namespace));
   end;

   function C_Raptor_Namespaces_Start_Namespace_Full (Stack: Namespace_Stack_Handle_Type;
                                                      Prefix: chars_ptr;
                                                      URI: chars_ptr;
                                                      Depth: int)
                                                      return Int
     with Import, Convention=>C, External_Name=>"raptor_namespaces_start_namespace_full";

   procedure Start_Namespace (Stack: Namespace_Stack_Type_Without_Finalize;
                              Prefix: String_Holders.Holder;
                              NS_URI: String_Holders.Holder;
                              Depth: Natural) is
      Prefix_N : C_String_Holder := To_C_String_Holder(Prefix);
      URI_N    : C_String_Holder := To_C_String_Holder(NS_URI);
   begin
      if C_Raptor_Namespaces_Start_Namespace_Full(Get_Handle(Stack), C_String(Prefix_N), C_String(URI_N), int(Depth)) /= 0 then
         raise Constraint_Error; -- TODO: Which exception to use?
      end if;
   end;

   procedure C_Raptor_Namespaces_End_For_Depth (Stack: Namespace_Stack_Handle_Type; Depth: int)
     with Import, Convention=>C, External_Name=>"raptor_namespaces_end_for_depth";

   procedure End_For_Depth (Stack: Namespace_Stack_Type_Without_Finalize; Depth: Natural) is
   begin
      C_Raptor_Namespaces_End_For_Depth(Get_Handle(Stack), Int(Depth));
   end;

   function C_raptor_namespaces_get_default_namespace (Stack: Namespace_Stack_Handle_Type) return Namespace_Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_namespaces_get_default_namespace";

   function Get_Default_Namespace (Stack: Namespace_Stack_Type_Without_Finalize'Class) return Namespace_Type is
   begin
      return From_Non_Null_Handle( C_Raptor_Namespaces_Get_Default_Namespace(Get_Handle(Stack)) );
   end;

end RDF.Raptor.Namespaces;
