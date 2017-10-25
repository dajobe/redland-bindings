with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary;
with RDF.Auxiliary.Handled_Record;
with RDF.Rasqal.World; use RDF.Rasqal.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.IOStream; use RDF.Raptor.IOStream;

package RDF.Rasqal.Data_Graph is

   type Data_Graph_Record is private;

   type Data_Graph_Record_Access is access Data_Graph_Record with Convention=>C;

   package Data_Graph_Handled_Record is new RDF.Auxiliary.Handled_Record(Data_Graph_Record, Data_Graph_Record_Access);

   type Data_Graph_Type_Without_Finalize is new Data_Graph_Handled_Record.Base_Object with null record;

   subtype Data_Graph_Handle is Data_Graph_Handled_Record.Access_Type;

   overriding function Adjust_Handle (Object: Data_Graph_Type_Without_Finalize; Handle: Data_Graph_Handle) return Data_Graph_Handle;

   overriding procedure Finalize_Handle (Object: Data_Graph_Type_Without_Finalize; Handle: Data_Graph_Handle);

   function Get_World (Graph: Data_Graph_Type_Without_Finalize)
                       return Rasqal_World_Type_Without_Finalize;

   function Get_URI (Graph: Data_Graph_Type_Without_Finalize) return URI_Type_Without_Finalize;

   function Get_Name_URI (Graph: Data_Graph_Type_Without_Finalize)
                          return URI_Type_Without_Finalize;

   type Flags_Type is (None, -- unused
                       Named,
                       Background)
     with Convention=>C;

   function Get_Flags (Graph: Data_Graph_Type_Without_Finalize) return Flags_Type;

   function Get_Format_Type (Graph: Data_Graph_Type_Without_Finalize) return String;

   function Get_Format_Name (Graph: Data_Graph_Type_Without_Finalize) return String;

   function Get_Format_URI (Graph: Data_Graph_Type_Without_Finalize) return URI_Type;

   function Get_IOStream (Graph: Data_Graph_Type_Without_Finalize) return IOStream_Type_Without_Finalize;

   function Get_Base_URI (Graph: Data_Graph_Type_Without_Finalize) return URI_Type;

   function Get_Usage_Count (Graph: Data_Graph_Type_Without_Finalize) return Natural;

--     not overriding function Copy (Object: Data_Graph_Type_Without_Finalize'Class)
--                                   return Data_Graph_Type_Without_Finalize;

   not overriding procedure Print (Graph: Data_Graph_Type_Without_Finalize;
                                   File: RDF.Auxiliary.C_File_Access);

   package Handlers is new Data_Graph_Handled_Record.Common_Handlers(Data_Graph_Type_Without_Finalize);

   type Data_Graph_Type is new Handlers.Base_With_Finalization with null record;

   not overriding function From_IOStream (World: Rasqal_World_Type_Without_Finalize'Class;
                                          IOStream: IOStream_Type_Without_Finalize'Class;
                                          Base_URI: URI_Type_Without_Finalize'Class;
                                          Name_URI: URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null));
                                          Flags: Flags_Type := Background;
                                          Format_Type, Format_Name: RDF.Auxiliary.String_Holders.Holder := RDF.Auxiliary.String_Holders.Empty_Holder;
                                          Format_URI: URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null)))
                                          return Data_Graph_Type;

   not overriding function From_URI (World: Rasqal_World_Type_Without_Finalize'Class;
                                     URI, Name_URI: URI_Type_Without_Finalize'Class;
                                     Flags: Flags_Type;
                                     Format_Type, Format_Name: RDF.Auxiliary.String_Holders.Holder;
                                     Format_URI: URI_Type_Without_Finalize'Class)
                                     return Data_Graph_Type;

   --     type Streamed_Data_Graph_Type is new Data_Graph_Type with private;

   --     -- Not binding, but a wrapper
   --     not overriding function From_File (World: Rasqal_World_Type_Without_Finalize'Class;
   --                                        Filename: String;
   --                                        Base_URI: URI_Type_Without_Finalize'Class;
   --                                        Name_URI: URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null));
   --                                        Flags: Flags_Type := Background;
   --                                        Format_Type, Format_Name: RDF.Auxiliary.String_Holders.Holder := RDF.Auxiliary.String_Holders.Empty_Holder;
   --                                        Format_URI: URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null)))
   --                                        return Streamed_Data_Graph_Type;
   --
   --     -- Not binding, but a wrapper
   --     not overriding function From_String (World: Rasqal_World_Type_Without_Finalize'Class;
   --                                          Str: String;
   --                                          Base_URI: URI_Type_Without_Finalize'Class;
   --                                          Name_URI: URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null));
   --                                          Flags: Flags_Type := Background;
   --                                          Format_Type, Format_Name: RDF.Auxiliary.String_Holders.Holder := RDF.Auxiliary.String_Holders.Empty_Holder;
   --                                          Format_URI: URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null)))
   --                                          return Streamed_Data_Graph_Type;

private

   type Data_Graph_Record is
      record
         World: Rasqal_World_Handle;
         URI, Name_URI: URI_Handle;
         Flags: unsigned;
         Format_Type, Format_Name: chars_ptr;
         Format_URI: URI_Handle;
         IOStr: IOStream_Handle;
         Base_URI: URI_Handle;
         Usage: int;
      end record
     with Convention=>C;

   --     type Streamed_Data_Graph_Type is new Data_Graph_Type with
   --        record
   --           Stream: Base_Stream_Type'Class;
   --        end record;

end RDF.Rasqal.Data_Graph;
