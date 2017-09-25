with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary; use RDF.Auxiliary;
with RDF.Auxiliary.C_String_Holders; use RDF.Auxiliary.C_String_Holders;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Namespace; use RDF.Raptor.Namespace;
with RDF.Auxiliary.Convert; use RDF.Auxiliary.Convert;

package body RDF.Raptor.Namespace_Stack is

   procedure raptor_namespaces_clear (Stack: Namespace_Stack_Handle)
     with Import, Convention=>C;

   procedure Clear (Stack: Namespace_Stack_Type_Without_Finalize) is
   begin
      raptor_namespaces_clear(Get_Handle(Stack));
   end;

   procedure raptor_free_namespaces (Stack: Namespace_Stack_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle(Object: Namespace_Stack_Type; Handle: Namespace_Stack_Handle) is
   begin
      raptor_free_namespaces(Handle);
   end;

   function raptor_namespaces_start_namespace_full (Stack: Namespace_Stack_Handle;
                                                      Prefix: chars_ptr;
                                                      URI: chars_ptr;
                                                      Depth: int)
                                                      return Int
     with Import, Convention=>C;

   procedure Start_Namespace (Stack: Namespace_Stack_Type_Without_Finalize;
                              Prefix: String_Holders.Holder;
                              NS_URI: String_Holders.Holder;
                              Depth: Natural) is
      Prefix_N : C_String_Holder := To_C_String_Holder(Prefix);
      URI_N    : C_String_Holder := To_C_String_Holder(NS_URI);
   begin
      if raptor_namespaces_start_namespace_full(Get_Handle(Stack), C_String(Prefix_N), C_String(URI_N), int(Depth)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   procedure raptor_namespaces_end_for_depth (Stack: Namespace_Stack_Handle; Depth: int)
     with Import, Convention=>C;

   procedure End_For_Depth (Stack: Namespace_Stack_Type_Without_Finalize; Depth: Natural) is
   begin
      raptor_namespaces_end_for_depth(Get_Handle(Stack), Int(Depth));
   end;

   function raptor_namespaces_get_default_namespace (Stack: Namespace_Stack_Handle) return Namespace_Handle
     with Import, Convention=>C;

   function Get_Default_Namespace (Stack: Namespace_Stack_Type_Without_Finalize) return Namespace_Type is
   begin
      return From_Non_Null_Handle( raptor_namespaces_get_default_namespace(Get_Handle(Stack)) );
   end;

   procedure raptor_namespaces_start_namespace (Stack: Namespace_Stack_Handle; Namespace: Namespace_Handle)
     with Import, Convention=>C;

   procedure Start_Namespace (Stack: Namespace_Stack_Type_Without_Finalize; Namespace: Namespace_Type'Class) is
   begin
      raptor_namespaces_start_namespace(Get_Handle(Stack), Get_Handle(Namespace));
   end;

   function raptor_new_namespaces (World: RDF.Raptor.World.Raptor_World_Handle;
                                     Defaults: Defaults_Type)
                                     return Namespace_Stack_Handle
     with Import, Convention=>C;

   function Create_Stack (World: RDF.Raptor.World.Raptor_World_Type_Without_Finalize'Class; Defaults: Defaults_Type)
                          return Namespace_Stack_Type is
   begin
      return From_Non_Null_Handle( raptor_new_namespaces(Get_Handle(World), Defaults) );
   end;

   function raptor_namespaces_find_namespace (Stack: Namespace_Stack_Handle;
                                                Prefix: chars_ptr;
                                                Len: int)
                                                return Namespace_Handle
     with Import, Convention=>C;

   function Find_Namespace (Stack: Namespace_Stack_Type_Without_Finalize; Prefix: String) return RDF.Raptor.Namespace.Namespace_Type is
      C_Prefix: aliased char_array := My_To_C_Without_Nul(Prefix);
   begin
      return From_Non_Null_Handle( raptor_namespaces_find_namespace(Get_Handle(Stack), To_Chars_Ptr(C_Prefix'Unchecked_Access), Prefix'Length) );
   end;

   function Find_Default_Namespace (Stack: Namespace_Stack_Type_Without_Finalize) return RDF.Raptor.Namespace.Namespace_Type is
   begin
      return From_Non_Null_Handle( raptor_namespaces_find_namespace(Get_Handle(Stack), Null_Ptr, 0) );
   end;

   function raptor_namespaces_find_namespace_by_uri (Stack: Namespace_Stack_Handle;
                                                       URI: RDF.Raptor.URI.URI_Handle)
                                                       return Namespace_Handle
     with Import, Convention=>C;

   function Find_Namespace_By_URI (Stack: Namespace_Stack_Type_Without_Finalize; URI: URI_Type_Without_Finalize'Class)
                                   return RDF.Raptor.Namespace.Namespace_Type is
   begin
      return From_Non_Null_Handle( raptor_namespaces_find_namespace_by_uri(Get_Handle(Stack), Get_Handle(URI)) ) ;
   end;

   function raptor_namespaces_namespace_in_scope (Stack: Namespace_Stack_Handle;
                                                    NS: Namespace_Handle)
                                                    return int
     with Import, Convention=>C;

   function In_Scope (Stack: Namespace_Stack_Type_Without_Finalize; NS: RDF.Raptor.Namespace.Namespace_Type'Class)
                      return Boolean is
   begin
      return raptor_namespaces_namespace_in_scope(Get_Handle(Stack), Get_Handle(NS)) /= 0;
   end;

   function raptor_namespace_stack_start_namespace (Stack: Namespace_Stack_Handle;
                                                      NS: Namespace_Handle;
                                                      New_Depth: int)
                                                      return Int
     with Import, Convention=>C;

   procedure Start_Namespace (Stack: Namespace_Stack_Type_Without_Finalize; NS: RDF.Raptor.Namespace.Namespace_Type_Without_Finalize'Class; New_Depth: Natural) is
   begin
      if raptor_namespace_stack_start_namespace(Get_Handle(Stack), Get_Handle(NS), int(New_Depth)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

end RDF.Raptor.Namespace_Stack;
