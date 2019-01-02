with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Auxiliary; use RDF.Auxiliary;
with RDF.Raptor.Namespace;

package RDF.Raptor.Namespace_Stack is

   package Namespace_Stack_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype Namespace_Stack_Handle is Namespace_Stack_Handled_Record.Access_Type;

   type Namespace_Stack_Type_Without_Finalize is new Namespace_Stack_Handled_Record.Base_Object with null record;

   overriding procedure Finalize_Handle (Object: Namespace_Stack_Type_Without_Finalize; Handle: Namespace_Stack_Handle);

   not overriding procedure Clear (Stack: in out Namespace_Stack_Type_Without_Finalize);

   -- See also below
   -- FIXME: Shouldn't it be Namespace_Type_Without_Finalize'Class?
   not overriding procedure Start_Namespace (Stack: in out Namespace_Stack_Type_Without_Finalize;
                                             Namespace: RDF.Raptor.Namespace.Namespace_Type'Class);

   not overriding procedure Start_Namespace (Stack: in out Namespace_Stack_Type_Without_Finalize;
                                             Prefix: String_Holders.Holder;
                                             NS_URI: String_Holders.Holder;
                                             Depth: Natural);

   not overriding procedure End_For_Depth (Stack: in out Namespace_Stack_Type_Without_Finalize; Depth: Natural);

   -- FIXME: Should be Namespace_Type_Without_Finalize?
   not overriding function Get_Default_Namespace (Stack: Namespace_Stack_Type_Without_Finalize)
                                                  return RDF.Raptor.Namespace.Namespace_Type;

   -- FIXME: Should be Namespace_Type_Without_Finalize?
   not overriding function Find_Namespace (Stack: Namespace_Stack_Type_Without_Finalize; Prefix: String)
                                           return RDF.Raptor.Namespace.Namespace_Type;

   -- FIXME: Should be Namespace_Type_Without_Finalize?
   not overriding function Find_Default_Namespace (Stack: Namespace_Stack_Type_Without_Finalize)
                                                   return RDF.Raptor.Namespace.Namespace_Type;

   -- FIXME: Should be Namespace_Type_Without_Finalize?
   not overriding function Find_Namespace_By_URI (Stack: Namespace_Stack_Type_Without_Finalize;
                                                  URI: URI_Type_Without_Finalize'Class)
                                                  return RDF.Raptor.Namespace.Namespace_Type;

   not overriding function In_Scope (Stack: Namespace_Stack_Type_Without_Finalize;
                                     NS: RDF.Raptor.Namespace.Namespace_Type'Class)
                                     return Boolean;

   not overriding procedure Start_Namespace (Stack: in out Namespace_Stack_Type_Without_Finalize;
                                             NS: RDF.Raptor.Namespace.Namespace_Type_Without_Finalize'Class;
                                             New_Depth: Natural);

   package Handlers is new Namespace_Stack_Handled_Record.Common_Handlers(Namespace_Stack_Type_Without_Finalize);

   type Namespace_Stack_Type is new Handlers.Base_With_Finalization with null record;

   type Namespace_Stack_Type_User is new Handlers.User_Type with null record;

   type Defaults_Type is (None_Type, XML_Type, RDF_Type, Undefined_Type)
     with Convention => C;
   for Defaults_Type use (None_Type=>0, XML_Type=>1, RDF_Type=>2, Undefined_Type=>3);

   not overriding function Create_Stack (World: Raptor_World_Type_Without_Finalize'Class;
                                         Defaults: Defaults_Type)
                                         return Namespace_Stack_Type;

   -- raptor_namespaces_init() not bound (it seems that function is internal for Raptor implementation).

end RDF.Raptor.Namespace_Stack;
