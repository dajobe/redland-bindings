with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxilary; use RDF.Auxilary;
with RDF.Auxilary.C_String_Holders; use RDF.Auxilary.C_String_Holders;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Namespaces; use RDF.Raptor.Namespaces;

package body RDF.Raptor.Namespaces_Stacks is

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
         raise RDF.Auxilary.RDF_Exception;
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

   function Get_Default_Namespace (Stack: Namespace_Stack_Type_Without_Finalize) return Namespace_Type is
   begin
      return From_Non_Null_Handle( C_Raptor_Namespaces_Get_Default_Namespace(Get_Handle(Stack)) );
   end;

   procedure C_Raptor_Namespaces_Start_Namespace (Stack: Namespace_Stack_Handle_Type; Namespace: Namespace_Handle_Type)
     with Import, Convention=>C, External_Name=>"raptor_namespaces_start_namespace";

   procedure Start_Namespace (Stack: Namespace_Stack_Type_Without_Finalize; Namespace: Namespace_Type'Class) is
   begin
      C_Raptor_Namespaces_Start_Namespace(Get_Handle(Stack), Get_Handle(Namespace));
   end;

   function C_Raptor_New_Namespaces (World: RDF.Raptor.World.Handle_Type;
                                     Defaults: Defaults_Type)
                                     return Namespace_Stack_Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_namespaces";

   function Create_Stack (World: RDF.Raptor.World.World_Type_Without_Finalize'Class; Defaults: Defaults_Type)
                          return Namespace_Stack_Type is
   begin
      return From_Non_Null_Handle( C_Raptor_New_Namespaces(Get_Handle(World), Defaults) );
   end;

   function C_Raptor_Namespaces_Find_Namespace (Stack: Namespace_Stack_Handle_Type;
                                                Prefix: Char_Array_Access;
                                                Len: int)
                                                return Namespace_Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_namespaces_find_namespace";

   function Find_Namespace (Stack: Namespace_Stack_Type_Without_Finalize; Prefix: String) return RDF.Raptor.Namespaces.Namespace_Type is
      C_Prefix: aliased char_array := To_C(Prefix, Append_Nul=>False);
   begin
      return From_Non_Null_Handle( C_Raptor_Namespaces_Find_Namespace(Get_Handle(Stack), C_Prefix'Unchecked_Access, Prefix'Length) );
   end;

   function Find_Default_Namespace (Stack: Namespace_Stack_Type_Without_Finalize) return RDF.Raptor.Namespaces.Namespace_Type is
   begin
      return From_Non_Null_Handle( C_Raptor_Namespaces_Find_Namespace(Get_Handle(Stack), null, 0) );
   end;

   function C_raptor_namespaces_find_namespace_by_uri (Stack: Namespace_Stack_Handle_Type;
                                                       URI: RDF.Raptor.URI.Handle_Type)
                                                       return Namespace_Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_namespaces_find_namespace_by_uri";

   function Find_Namespace_By_URI (Stack: Namespace_Stack_Type_Without_Finalize; URI: URI_Type_Without_Finalize'Class)
                                   return RDF.Raptor.Namespaces.Namespace_Type is
   begin
      return From_Non_Null_Handle( C_Raptor_Namespaces_Find_Namespace_By_Uri(Get_Handle(Stack), Get_Handle(URI)) ) ;
   end;

   function C_Raptor_Namespaces_Namespace_In_Scope (Stack: Namespace_Stack_Handle_Type;
                                                    NS: Namespace_Handle_Type)
                                                    return int
     with Import, Convention=>C, External_Name=>"raptor_namespaces_namespace_in_scope";

   function In_Scope (Stack: Namespace_Stack_Type_Without_Finalize; NS: RDF.Raptor.Namespaces.Namespace_Type'Class)
                      return Boolean is
   begin
      return C_Raptor_Namespaces_Namespace_In_Scope(Get_Handle(Stack), Get_Handle(NS)) /= 0;
   end;

   function C_Raptor_Namespace_Stack_Start_Namespace (Stack: Namespace_Stack_Handle_Type;
                                                      NS: Namespace_Handle_Type;
                                                      New_Depth: int)
                                                      return Int
     with Import, Convention=>C, External_Name=>"raptor_namespace_stack_start_namespace";

   procedure Start_Namespace (Stack: Namespace_Stack_Type_Without_Finalize; NS: RDF.Raptor.Namespaces.Namespace_Type_Without_Finalize'Class; New_Depth: Natural) is
   begin
      if C_Raptor_Namespace_Stack_Start_Namespace(Get_Handle(Stack), Get_Handle(NS), int(New_Depth)) /= 0 then
         raise RDF.Auxilary.RDF_Exception;
      end if;
   end;

end RDF.Raptor.Namespaces_Stacks;
