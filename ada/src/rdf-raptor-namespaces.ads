with RDF.Auxilary.Limited_Handled_Record;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with Interfaces.C;
with RDF.Raptor.World;
with RDF.Auxilary; use RDF.Auxilary;

package RDF.Raptor.Namespaces is

   package Namespace_Handled_Record is new RDF.Auxilary.Limited_Handled_Record(RDF.Auxilary.Dummy_Record);

   subtype Namespace_Handle_Type is Namespace_Handled_Record.Access_Type;

   package Namespace_Stack_Handled_Record is new RDF.Auxilary.Limited_Handled_Record(RDF.Auxilary.Dummy_Record);

   subtype Namespace_Stack_Handle_Type is Namespace_Stack_Handled_Record.Access_Type;

   type Namespace_Type_Without_Finalize is new Namespace_Handled_Record.Base_Object with null record;

   type Namespace_Stack_Type_Without_Finalize is new Namespace_Stack_Handled_Record.Base_Object with null record;

   type Namespace_Type is new Namespace_Type_Without_Finalize with null record;

   not overriding procedure Clear (Stack: Namespace_Stack_Type_Without_Finalize);

   not overriding procedure Start_Namespace (Stack: Namespace_Stack_Type_Without_Finalize; Namespace: Namespace_Type'Class);

   not overriding procedure Start_Namespace (Stack: Namespace_Stack_Type_Without_Finalize;
                                             Prefix: String_Holders.Holder;
                                             NS_URI: String_Holders.Holder;
                                             Depth: Natural);

   not overriding procedure End_For_Depth (Stack: Namespace_Stack_Type_Without_Finalize; Depth: Natural);

   -- 'Class is a hack, because otherwise this function would be dispatching on two types
   not overriding function Get_Default_Namespace (Stack: Namespace_Stack_Type_Without_Finalize'Class) return Namespace_Type;

   type Namespace_Stack_Type is new Namespace_Stack_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle(Object: Namespace_Stack_Type; Handle: Namespace_Stack_Handle_Type);

   not overriding function From_URI (Stack: Namespace_Stack_Type_Without_Finalize'Class;
                                     Prefix: String;
                                     URI: URI_Type_Without_Finalize'Class;
                                     Depth: Integer)
                                     return Namespace_Type;

   type Defaults_Type is (None_Type, XML_Type, RDF_Type, Undefined_Type);
   for Defaults_Type'Size use Interfaces.C.int'Size; -- hack
   for Defaults_Type use (None_Type=>0, XML_Type=>1, RDF_Type=>2, Undefined_Type=>3);

   not overriding function Create_Stack (World: RDF.Raptor.World.World_Type_Without_Finalize'Class; Defaults: Defaults_Type)
                                         return Namespace_Stack_Type;

   -- raptor_namespaces_init() not bound (it seems that function is internal for Raptor implementation).

--     function Find_Namespace

   -- TODO: stopped at raptor_namespaces_find_namespace()

end RDF.Raptor.Namespaces;
