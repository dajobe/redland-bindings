with RDF.Auxilary.Limited_Handled_Record;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with Interfaces.C;
with RDF.Raptor.World;
with RDF.Auxilary; use RDF.Auxilary;
limited with RDF.Raptor.Namespaces_Stacks;

package RDF.Raptor.Namespaces is

   package Namespace_Handled_Record is new RDF.Auxilary.Limited_Handled_Record(RDF.Auxilary.Dummy_Record);

   subtype Namespace_Handle_Type is Namespace_Handled_Record.Access_Type;

   type Namespace_Type_Without_Finalize is new Namespace_Handled_Record.Base_Object with null record;

   type Namespace_Type is new Namespace_Type_Without_Finalize with null record;

   not overriding function From_URI (Stack: RDF.Raptor.Namespaces_Stacks.Namespace_Stack_Type_Without_Finalize'Class;
                                     Prefix: String;
                                     URI: URI_Type_Without_Finalize'Class;
                                     Depth: Integer)
                                     return Namespace_Type;

   -- raptor_namespaces_init() not bound (it seems that function is internal for Raptor implementation).

--     function Find_Namespace

   -- TODO: stopped at raptor_namespaces_find_namespace()

end RDF.Raptor.Namespaces;
