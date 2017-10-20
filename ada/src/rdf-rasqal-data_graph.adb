with RDF.Auxiliary.C_String_Holders;

package body RDF.Rasqal.Data_Graph is

   function Get_World (Graph: Data_Graph_Type_Without_Finalize)
                       return Rasqal_World_Type_Without_Finalize is
   begin
      return From_Handle(Get_Handle(Graph).World);
   end;

   function Get_URI (Graph: Data_Graph_Type_Without_Finalize)
                     return URI_Type_Without_Finalize is
   begin
      return From_Handle(Get_Handle(Graph).URI);
   end;

   function Get_Name_URI (Graph: Data_Graph_Type_Without_Finalize)
                          return URI_Type_Without_Finalize is
   begin
      return From_Handle(Get_Handle(Graph).Name_URI);
   end;

   function Get_Flags (Graph: Data_Graph_Type_Without_Finalize) return Flags_Type is
     (Flags_Type'Val(Get_Handle(Graph).Flags));

   function Get_Format_Type (Graph: Data_Graph_Type_Without_Finalize) return String is
     (Value(Get_Handle(Graph).Format_Type));

   function Get_Format_Name (Graph: Data_Graph_Type_Without_Finalize) return String is
     (Value(Get_Handle(Graph).Format_Name));


   function Get_Format_URI (Graph: Data_Graph_Type_Without_Finalize) return URI_Type is
   begin
      return From_Handle(Get_Handle(Graph).Format_URI);
   end;

   function Get_IOStream (Graph: Data_Graph_Type_Without_Finalize)
                          return IOStream_Type_Without_Finalize is
   begin
      return From_Handle(Get_Handle(Graph).IOStr);
   end;

   function Get_Base_URI (Graph: Data_Graph_Type_Without_Finalize) return URI_Type is
   begin
      return From_Handle(Get_Handle(Graph).Base_URI);
   end;

   function Get_Usage_Count (Graph: Data_Graph_Type_Without_Finalize) return Natural is
     (Natural(Get_Handle(Graph).Usage));

   function rasqal_new_data_graph_from_data_graph (Graph: Data_Graph_Handle)
                                                   return Data_Graph_Handle
     with Import, Convention=>C;

   function Adjust_Handle (Object: Data_Graph_Type_Without_Finalize; Handle: Data_Graph_Handle)
                            return Data_Graph_Handle is
   begin
      -- FIXME: rasqal_new_data_graph_from_data_graph() accepts NULL
      return rasqal_new_data_graph_from_data_graph(Handle);
   end;

--     function Copy (Object: Data_Graph_Type_Without_Finalize'Class)
--                    return Data_Graph_Type_Without_Finalize is
--     begin
--        return From_Handle(rasqal_new_data_graph_from_data_graph(Get_Handle(Object)));
--     end;

   procedure rasqal_free_data_graph (Handle: Data_Graph_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Data_Graph_Type_Without_Finalize; Handle: Data_Graph_Handle) is
   begin
      rasqal_free_data_graph(Handle);
   end;

   function rasqal_new_data_graph_from_iostream (World: Rasqal_World_Handle;
                                                 IOStream: IOStream_Handle;
                                                 Base_URI, Name_URI: URI_Handle;
                                                 Flags: unsigned;
                                                 Format_Type, Format_Name: chars_ptr;
                                                 Format_URI: URI_Handle)
                                                 return Data_Graph_Handle
     with Import, Convention=>C;

   function From_IOStream (World: Rasqal_World_Type_Without_Finalize'Class;
                           IOStream: Base_IOStream_Type'Class;
                           Base_URI: URI_Type_Without_Finalize'Class;
                           Name_URI: URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null));
                           Flags: Flags_Type := Background;
                           Format_Type, Format_Name: RDF.Auxiliary.String_Holders.Holder := RDF.Auxiliary.String_Holders.Empty_Holder;
                           Format_URI: URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null)))
                           return Data_Graph_Type is
      use RDF.Auxiliary.C_String_Holders;
      Format_Type2: chars_ptr := New_String(Format_Type);
      Format_Name2: chars_ptr := New_String(Format_Name);
      Result: constant Data_Graph_Handle :=
        rasqal_new_data_graph_from_iostream(Get_Handle(World),
                                            Get_Handle(IOStream),
                                            Get_Handle(Base_URI),
                                            Get_Handle(Name_URI),
                                            Flags_Type'Pos(Flags),
                                            Format_Type2,
                                            Format_Name2,
                                            Get_Handle(Format_URI));
   begin
      Free(Format_Type2);
      Free(Format_Name2);
      return From_Handle(Result);
   end;

   function rasqal_new_data_graph_from_uri (World: Rasqal_World_Handle;
                                            URI, Name_URI: URI_Handle;
                                            Flags: Unsigned;
                                            Format_Type, Format_Name: Chars_Ptr;
                                            Format_URI: URI_Handle)
                                            return Data_Graph_Handle
     with Import, Convention=>C;

   function From_URI (World: Rasqal_World_Type_Without_Finalize'Class;
                      URI, Name_URI: URI_Type_Without_Finalize'Class;
                      Flags: Flags_Type;
                      Format_Type, Format_Name: RDF.Auxiliary.String_Holders.Holder;
                      Format_URI: URI_Type_Without_Finalize'Class)
                      return Data_Graph_Type is
      use RDF.Auxiliary.C_String_Holders;
      Format_Type2: chars_ptr := New_String(Format_Type);
      Format_Name2: chars_ptr := New_String(Format_Name);
      Result: constant Data_Graph_Handle :=
        rasqal_new_data_graph_from_uri(Get_Handle(World),
                                       Get_Handle(URI),
                                       Get_Handle(Name_URI),
                                       Flags_Type'Pos(Flags),
                                       Format_Type2,
                                       Format_Name2,
                                       Get_Handle(Format_URI));
   begin
      Free(Format_Type2);
      Free(Format_Name2);
      return From_Handle(Result);
   end;

   --     function From_File (World: Rasqal_World_Type_Without_Finalize'Class;
   --                         Filename: String;
   --                         Base_URI: URI_Type_Without_Finalize'Class;
   --                         Name_URI: URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null));
   --                         Flags: Flags_Type := Background;
   --                         Format_Type, Format_Name: RDF.Auxiliary.String_Holders.Holder := RDF.Auxiliary.String_Holders.Empty_Holder;
   --                         Format_URI: URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null)))
   --                         return Data_Graph_Type is
   --        use RDF.Rasqal.World;
   --        --        Stream: Stream_From_String := Open_From_String(Get_Raptor(World), Str);
   --        Str: String := TODO;
   --     begin
   --        return From_String (World, Str, Base_URI, Name_URI, Flags, Format_Type, Format_Name, Format_URI);
   --     end;
   --
   --     function From_String (World: Rasqal_World_Type_Without_Finalize'Class;
   --                           Str: String;
   --                           Base_URI: URI_Type_Without_Finalize'Class;
   --                           Name_URI: URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null));
   --                           Flags: Flags_Type := Background;
   --                           Format_Type, Format_Name: RDF.Auxiliary.String_Holders.Holder := RDF.Auxiliary.String_Holders.Empty_Holder;
   --                           Format_URI: URI_Type_Without_Finalize'Class := URI_Type'(From_Handle(null)))
   --                           return Data_Graph_Type is
   --        use RDF.Raptor.IOStream, RDF.Rasqal.World;
   --     begin
   --        -- Does not work if Stream variable is destroyed:
   --  --        return From_IOStream(World, Stream, Base_URI, Name_URI, Flags, Format_Type, Format_Name, Format_URI);
   --  --        return From_String(World, Str, Base_URI, Name_URI, Flags, Format_Type, Format_Name, Format_URI);
   --     end;

   function rasqal_data_graph_print (Graph: Data_Graph_Handle; File: RDF.Auxiliary.C_File_Access)
                                     return int
     with Import, Convention=>C;

   procedure Print (Graph: Data_Graph_Type_Without_Finalize; File: RDF.Auxiliary.C_File_Access) is
   begin
      if rasqal_data_graph_print(Get_Handle(Graph), File) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

end RDF.Rasqal.Data_Graph;
