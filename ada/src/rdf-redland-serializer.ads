with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Raptor.IOStream; use RDF.Raptor.IOStream;
with RDF.Raptor.Syntaxes; use RDF.Raptor.Syntaxes;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.URI; use RDF.Redland.URI;
with RDF.Redland.Stream; use RDF.Redland.Stream;
with RDF.Redland.Model; use RDF.Redland.Model;

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

   function Serializer_Check_Name (World: Redland_World_Type_Without_Finalize'Class;
                                   Name: String)
                                   return Boolean;

   -- Order of arguments not the same as in C
   not overriding
   procedure Serialize_To_File_Handle (Serializer: Serializer_Type_Without_Finalize;
                                       File: RDF.Auxiliary.C_File_Access;
                                       Model: Model_Type_Without_Finalize'Class;
                                       Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)));

   -- Order of arguments not the same as in C
   not overriding
   procedure Serialize_To_File (Serializer: Serializer_Type_Without_Finalize;
                                File_Name: String;
                                Model: Model_Type_Without_Finalize'Class;
                                Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)));

   -- Order of arguments not the same as in C
   not overriding function Serialize_To_String (Serializer: Serializer_Type_Without_Finalize;
                                                Model: Model_Type_Without_Finalize'Class;
                                                Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                                                return String;

   -- Order of arguments not the same as in C
   not overriding procedure Serialize_Model_To_IOStream
     (Serializer: Serializer_Type_Without_Finalize;
      Model: Model_Type_Without_Finalize'Class;
      IOStream: Base_IOStream_Type'Class;
      Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)));

   -- Order of arguments not the same as in C
   not overriding function Serialize_To_String (Serializer: Serializer_Type_Without_Finalize;
                                                Stream: Stream_Type_Without_Finalize'Class;
                                                Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                                                return String;

   -- Order of arguments not the same as in C
   not overriding
   procedure Serialize_To_File (Serializer: Serializer_Type_Without_Finalize;
                                File_Name: String;
                                Stream: Stream_Type_Without_Finalize'Class;
                                Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)));

   -- Order of arguments not the same as in C
   not overriding
   procedure Serialize_To_File_Handle (Serializer: Serializer_Type_Without_Finalize;
                                       File: RDF.Auxiliary.C_File_Access;
                                       Stream: Stream_Type_Without_Finalize'Class;
                                       Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)));

   -- Order of arguments not the same as in C
   not overriding
   procedure Serialize_To_IOStream (Serializer: Serializer_Type_Without_Finalize;
                                    File: Base_IOStream_Type'Class;
                                    Stream: Stream_Type_Without_Finalize'Class;
                                    Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)));

   -- TODO: http://bugs.librdf.org/mantis/view.php?id=641
--     not overriding function Get_Feature (Serializer: Serializer_Type_Without_Finalize;
--                                          Feature: URI_Type_Without_Finalize'Class);
--                                          return Node_Type;
--     not overriding function Set_Feature (Serializer: Serializer_Type_Without_Finalize;
--                                          Feature: URI_Type_Without_Finalize'Class;
--                                          Value: Node_Iterator_Type_Without_Finalize'Class);

   -- Stopped at librdf_serializer_get_feature()

   package Finalizer is new Serializer_Handled_Record.With_Finalization(Serializer_Type_Without_Finalize);

   type Serializer_Type is new Finalizer.Derived with null record;

   not overriding function Create (World: Redland_World_Type_Without_Finalize'Class;
                                   Name, Mime_Type: String := "";
                                   Type_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                                   return Serializer_Type;

end RDF.Redland.Serializer;
