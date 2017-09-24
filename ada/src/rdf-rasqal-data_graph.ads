with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary;
with RDF.Auxiliary.Handled_Record;
with RDF.Rasqal.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.IOStream;

package RDF.Rasqal.Data_Graph is

   type Data_Graph_Record is private;

   type Data_Graph_Record_Access is access Data_Graph_Record with Convention=>C;

   package Data_Graph_Handled_Record is new RDF.Auxiliary.Handled_Record(Data_Graph_Record, Data_Graph_Record_Access);

   type Data_Graph_Type_Without_Finalize is new Data_Graph_Handled_Record.Base_Object with null record;

   subtype Data_Graph_Handle is Data_Graph_Handled_Record.Access_Type;

   function Get_World (Graph: Data_Graph_Type_Without_Finalize) return RDF.Rasqal.World.Raptor_World_Type_Without_Finalize;

   function Get_URI (Graph: Data_Graph_Type_Without_Finalize) return RDF.Raptor.URI.URI_Type_Without_Finalize;

   function Get_Name_URI (Graph: Data_Graph_Type_Without_Finalize) return RDF.Raptor.URI.URI_Type_Without_Finalize;

   type Flags_Type is (None, -- unused
                       Named,
                       Background)
      with Convention=>C;

   function Get_Flags (Graph: Data_Graph_Type_Without_Finalize) return Flags_Type;

   function Get_Format_Type (Graph: Data_Graph_Type_Without_Finalize) return String;

   function Get_Format_Name (Graph: Data_Graph_Type_Without_Finalize) return String;

   function Get_Format_URI (Graph: Data_Graph_Type_Without_Finalize) return RDF.Raptor.URI.URI_Type;

   function Get_Iostream (Graph: Data_Graph_Type_Without_Finalize) return RDF.Raptor.IOStream.Stream_Type_Without_Finalize;

   function Get_Base_URI (Graph: Data_Graph_Type_Without_Finalize) return RDF.Raptor.URI.URI_Type;

   function Get_Usage_Count (Graph: Data_Graph_Type_Without_Finalize) return Natural;

   not overriding function Copy (Object: Data_Graph_Type_Without_Finalize'Class) return Data_Graph_Type_Without_Finalize;

   not overriding procedure Print (Graph: Data_Graph_Type_Without_Finalize; File: RDF.Auxiliary.C_File_Access);

   type Data_Graph_Type is new Data_Graph_Type_Without_Finalize with null record;

   overriding procedure Adjust (Object: in out Data_Graph_Type);

   overriding procedure Finalize_Handle (Object: Data_Graph_Type; Handle: Data_Graph_Handle);

   not overriding function From_IOStream (World: RDF.Rasqal.World.Raptor_World_Type_Without_Finalize'Class;
                                          IOStream: RDF.Raptor.IOStream.Base_Stream_Type'Class;
                                          Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class;
                                          Name_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null));
                                          Flags: Flags_Type := Background;
                                          Format_Type, Format_Name: RDF.Auxiliary.String_Holders.Holder := RDF.Auxiliary.String_Holders.Empty_Holder;
                                          Format_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null)))
                                          return Data_Graph_Type;

   not overriding function From_URI (World: RDF.Rasqal.World.Raptor_World_Type_Without_Finalize'Class;
                                     URI, Name_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class;
                                     Flags: Flags_Type;
                                     Format_Type, Format_Name: RDF.Auxiliary.String_Holders.Holder;
                                     Format_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class)
                                     return Data_Graph_Type;

--     type Streamed_Data_Graph_Type is new Data_Graph_Type with private;

   -- TODO: Implement
--     -- Not binding, but a wrapper
--     not overriding function From_File (World: RDF.Rasqal.World.Raptor_World_Type_Without_Finalize'Class;
--                                        Filename: String;
--                                        Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class;
--                                        Name_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null));
--                                        Flags: Flags_Type := Background;
--                                        Format_Type, Format_Name: RDF.Auxiliary.String_Holders.Holder := RDF.Auxiliary.String_Holders.Empty_Holder;
--                                        Format_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null)))
--                                        return Streamed_Data_Graph_Type;
--
--     -- Not binding, but a wrapper
--     not overriding function From_String (World: RDF.Rasqal.World.Raptor_World_Type_Without_Finalize'Class;
--                                          Str: String;
--                                          Base_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class;
--                                          Name_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null));
--                                          Flags: Flags_Type := Background;
--                                          Format_Type, Format_Name: RDF.Auxiliary.String_Holders.Holder := RDF.Auxiliary.String_Holders.Empty_Holder;
--                                          Format_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null)))
--                                          return Streamed_Data_Graph_Type;

private

   type Data_Graph_Record is
      record
         World: RDF.Rasqal.World.Handle_Type;
         URI, Name_URI: RDF.Raptor.URI.Handle_Type;
         Flags: unsigned;
         Format_Type, Format_Name: chars_ptr;
         Format_URI: RDF.Raptor.URI.Handle_Type;
         IOStr: RDF.Raptor.IOStream.Handle_Type;
         Base_URI: RDF.Raptor.URI.Handle_Type;
         Usage: int;
      end record
     with Convention=>C;

--     type Streamed_Data_Graph_Type is new Data_Graph_Type with
--        record
--           Stream: RDF.Raptor.IOStream.Base_Stream_Type'Class;
--        end record;

end RDF.Rasqal.Data_Graph;
