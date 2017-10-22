with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.Syntaxes; use RDF.Raptor.Syntaxes;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.URI; use RDF.Redland.URI;

package RDF.Redland.Serializer is

   package Serializer_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Serializer_Type_Without_Finalize is new RDF.Redland.Serializer.Serializer_Handled_Record.Base_Object with null record;

   subtype Serializer_Handle is Serializer_Handled_Record.Access_Type;

   -- TODO: Iterator of descriptions

   overriding procedure Finalize_Handle (Object: Serializer_Type_Without_Finalize;
                                         Handle: Serializer_Handle);

   not overriding
   function Get_Description (Serializer: Serializer_Type_Without_Finalize; Index: Natural)
                             return Raptor_Syntax_Description_Type;

   package Finalizer is new Serializer_Handled_Record.With_Finalization(Serializer_Type_Without_Finalize);

   type Serializer_Type is new Finalizer.Derived with null record;

   not overriding function Create (World: Redland_World_Type_Without_Finalize'Class;
                                   Name, Mime_Type: String := "";
                                   Type_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                                   return Serializer_Type;

   -- Stopped at librdf_serializer_check_name()

end RDF.Redland.Serializer;
