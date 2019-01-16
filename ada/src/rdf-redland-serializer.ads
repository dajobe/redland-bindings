with Ada.Iterator_Interfaces;
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

   overriding procedure Finalize_Handle (Object: Serializer_Type_Without_Finalize;
                                         Handle: Serializer_Handle);

   -- FIXME: 1. rename to Get_Serializer_Description; 2. use World not Serializer
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

   -- FIXME: Serialize_Model_To_IOStream -> Serialize_To_IOStream
   -- Order of arguments not the same as in C
   not overriding procedure Serialize_Model_To_IOStream
     (Serializer: Serializer_Type_Without_Finalize;
      Model: Model_Type_Without_Finalize'Class;
      IOStream: IOStream_Type_Without_Finalize'Class;
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
                                    File: IOStream_Type_Without_Finalize'Class;
                                    Stream: Stream_Type_Without_Finalize'Class;
                                    Base_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)));

   -- http://bugs.librdf.org/mantis/view.php?id=641
--     not overriding function Get_Feature (Serializer: Serializer_Type_Without_Finalize;
--                                          Feature: URI_Type_Without_Finalize'Class);
--                                          return Node_Type;
--     not overriding function Set_Feature (Serializer: Serializer_Type_Without_Finalize;
--                                          Feature: URI_Type_Without_Finalize'Class;
--                                          Value: Node_Iterator_Type_Without_Finalize'Class);

   type Serializer_Description_Cursor is private;

   function Get_Position (Cursor: Serializer_Description_Cursor) return Natural;

   function Get_Description (Cursor: Serializer_Description_Cursor) return Raptor_Syntax_Description_Type;

   function Has_Element (Position: Serializer_Description_Cursor) return Boolean;

   package Serializer_Description_Iterators is new Ada.Iterator_Interfaces(Serializer_Description_Cursor, Has_Element);

   type Serializer_Description_Iterator is new Serializer_Description_Iterators.Forward_Iterator with private;

   overriding function First (Object: Serializer_Description_Iterator) return Serializer_Description_Cursor;
   overriding function Next (Object: Serializer_Description_Iterator; Position: Serializer_Description_Cursor)
                             return Serializer_Description_Cursor;

   function Create_Serializer_Descriptions_Iterator (World: Redland_World_Type_Without_Finalize'Class)
                                                 return Serializer_Description_Iterator;

   package Handlers is new Serializer_Handled_Record.Common_Handlers(Serializer_Type_Without_Finalize);

   type Serializer_Type is new Handlers.Base_With_Finalization with null record;

   type Serializer_Type_User is new Handlers.User_Type with null record;

   not overriding function Create (World: Redland_World_Type_Without_Finalize'Class;
                                   Name, Mime_Type: String := "";
                                   Type_URI: URI_Type_Without_Finalize'Class := URI_Type_Without_Finalize'(From_Handle(null)))
                                   return Serializer_Type;

private

   type Serializer_Description_Cursor is
      record
         World: Redland_World_Handle;
         Position: Natural;
      end record;

   type Serializer_Description_Iterator is new Serializer_Description_Iterators.Forward_Iterator with
      record
         World: Redland_World_Handle;
      end record;

end RDF.Redland.Serializer;
