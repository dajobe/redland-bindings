with Interfaces.C; use Interfaces.C;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.Iostream; use RDF.Raptor.IOStream;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Namespace; use RDF.Raptor.Namespace;
with RDF.Raptor.Statement; use RDF.Raptor.Statement;
with RDF.Raptor.Syntaxes;
with RDF.Raptor.Log; use RDF.Raptor.Log;
with RDF.Raptor.Options; use RDF.Raptor.Options;

package RDF.Raptor.Serializer is

   package Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype Serializer_Handle is Handled_Record.Access_Type;

   type Serializer_Type_Without_Finalize is new Handled_Record.Base_Object with null record;

   -- WARNING: Other order of arguments than in C
   not overriding procedure Start_To_Iostream (Serializer: Serializer_Type_Without_Finalize;
                                               Iostream: Base_Stream_Type'Class;
                                               URI: URI_Type_Without_Finalize := From_Handle(null));

   not overriding procedure Start_To_Filename (Serializer: Serializer_Type_Without_Finalize; Filename: String);

   -- raptor_serializer_start_to_string() is deliberately not used.
   -- Use Stream_To_String instead.

   not overriding procedure Start_To_Filehandle (Serializer: Serializer_Type_Without_Finalize;
                                                 URI: URI_Type_Without_Finalize'Class;
                                                 FH: RDF.Auxiliary.C_File_Access);

   -- WARNING: Other order of arguments than in C
   not overriding procedure Set_Namespace (Serializer: Serializer_Type_Without_Finalize;
                                           Prefix: String;
                                           URI: URI_Type_Without_Finalize := From_Handle(null));

   not overriding procedure Set_Namespace_Without_Prefix (Serializer: Serializer_Type_Without_Finalize;
                                                          URI: URI_Type_Without_Finalize := From_Handle(null));

   not overriding procedure Set_Namespace (Serializer: Serializer_Type_Without_Finalize;
                                           Namespace: Namespace_Type_Without_Finalize);

   not overriding procedure Serialize_Statement (Serializer: Serializer_Type_Without_Finalize;
                                                 Statement: Statement_Type_Without_Finalize'Class);

   not overriding procedure Serialize_End (Serializer: Serializer_Type_Without_Finalize);

   not overriding procedure Serialize_Flush (Serializer: Serializer_Type_Without_Finalize);

   not overriding function Get_Description (Serializer: Serializer_Type_Without_Finalize) return RDF.Raptor.Syntaxes.Syntax_Description_Type;

   not overriding function Get_Iostream (Serializer: Serializer_Type_Without_Finalize) return Stream_Type_Without_Finalize;

   not overriding function Get_Locator (Serializer: Serializer_Type_Without_Finalize) return Locator_Type;

   not overriding procedure Set_Option (Serializer: Serializer_Type_Without_Finalize; Option: Raptor_Option; Value: String);
   not overriding procedure Set_Option (Serializer: Serializer_Type_Without_Finalize; Option: Raptor_Option; Value: int);

   -- Not sure if we should be able to query here whether the option is numeric
   not overriding function Get_Numeric_Option (Serializer: Serializer_Type_Without_Finalize; Option: Raptor_Option) return Natural;
   not overriding function Get_String_Option (Serializer: Serializer_Type_Without_Finalize; Option: Raptor_Option) return String;

   not overriding function Get_World (Serializer: Serializer_Type_Without_Finalize) return Raptor_World_Type_Without_Finalize;

   type Serializer_Type is new Serializer_Type_Without_Finalize with null record;

   not overriding function New_Serializer (World: Raptor_World_Type) return Serializer_Type;
   not overriding function New_Serializer (World: Raptor_World_Type; Syntax_Name: String) return Serializer_Type;

   overriding procedure Finalize_Handle (Serializer: Serializer_Type; Handle: Serializer_Handle);

end RDF.Raptor.Serializer;
