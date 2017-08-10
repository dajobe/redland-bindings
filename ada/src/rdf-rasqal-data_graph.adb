with RDF.Auxiliary.C_String_Holders;

package body RDF.Rasqal.Data_Graph is

   function Get_World (Graph: Data_Graph_Type_Without_Finalize)
                       return RDF.Rasqal.World.World_Type_Without_Finalize is
      use RDF.Rasqal.World;
   begin
      return From_Handle(Get_Handle(Graph).World);
   end;

   function Get_URI (Graph: Data_Graph_Type_Without_Finalize)
                     return RDF.Raptor.URI.URI_Type_Without_Finalize is
      use RDF.Raptor.URI;
   begin
      return From_Handle(Get_Handle(Graph).URI);
   end;

   function Get_Name_URI (Graph: Data_Graph_Type_Without_Finalize)
                     return RDF.Raptor.URI.URI_Type_Without_Finalize is
      use RDF.Raptor.URI;
   begin
      return From_Handle(Get_Handle(Graph).Name_URI);
   end;

   function Get_Flags (Graph: Data_Graph_Type_Without_Finalize) return Flags_Type is
     (Flags_Type'Val(Get_Handle(Graph).Flags));

   function Get_Format_Type (Graph: Data_Graph_Type_Without_Finalize) return String is
      (Value(Get_Handle(Graph).Format_Type));

   function Get_Format_Name (Graph: Data_Graph_Type_Without_Finalize) return String is
      (Value(Get_Handle(Graph).Format_Name));


   function Get_Format_URI (Graph: Data_Graph_Type_Without_Finalize) return RDF.Raptor.URI.URI_Type is
      use RDF.Raptor.URI;
   begin
      return From_Handle(Get_Handle(Graph).Format_URI);
   end;

   function Get_Iostream (Graph: Data_Graph_Type_Without_Finalize)
                          return RDF.Raptor.IOStream.Stream_Type_Without_Finalize is
      use RDF.Raptor.IOStream;
   begin
      return From_Handle(Get_Handle(Graph).IOStr);
   end;

   function Get_Base_URI (Graph: Data_Graph_Type_Without_Finalize) return RDF.Raptor.URI.URI_Type is
      use RDF.Raptor.URI;
   begin
      return From_Handle(Get_Handle(Graph).Base_URI);
   end;

   function Get_Usage_Count (Graph: Data_Graph_Type_Without_Finalize) return Natural is
      (Natural(Get_Handle(Graph).Usage));

   function rasqal_new_data_graph_from_data_graph (Graph: Data_Graph_Handle) return Data_Graph_Handle
     with Import, Convention=>C;

   procedure Adjust (Object: in out Data_Graph_Type) is
   begin
      Set_Handle_Hack(Object, rasqal_new_data_graph_from_data_graph(Get_Handle(Object)));
   end;

   function Copy (Object: Data_Graph_Type_Without_Finalize'Class) return Data_Graph_Type_Without_Finalize is
   begin
      return From_Handle(rasqal_new_data_graph_from_data_graph(Get_Handle(Object)));
   end;

   procedure rasqal_free_data_graph (Handle: Data_Graph_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Data_Graph_Type; Handle: Data_Graph_Handle) is
   begin
      rasqal_free_data_graph(Handle);
   end;

   function rasqal_new_data_graph_from_iostream (World: RDF.Rasqal.World.Handle_Type;
                                                 IOStream: RDF.Raptor.IOStream.Handle_Type;
                                                 Base_URI, Name_URI: RDF.Raptor.URI.Handle_Type;
                                                 Flags: unsigned;
                                                 Format_Type, Format_Name: chars_ptr;
                                                 Format_URI: RDF.Raptor.URI.Handle_Type)
                                                 return Data_Graph_Handle
     with Import, Convention=>C;

   function From_IOStream (World: RDF.Rasqal.World.World_Type_Without_Finalize'Class;
                           IOStream: RDF.Raptor.IOStream.Stream_Type_Without_Finalize'Class;
                           Base_URI, Name_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class;
                           Flags: Flags_Type;
                           Format_Type, Format_Name: RDF.Auxiliary.String_Holders.Holder;
                           Format_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class)
                           return Data_Graph_Type is
      use RDF.Auxiliary.C_String_Holders;
      Format_Type2: chars_ptr := New_String(Format_Type);
      Format_Name2: chars_ptr := New_String(Format_Name);
      use RDF.Rasqal.World, RDF.Raptor.IOStream, RDF.Raptor.URI;
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

   function rasqal_new_data_graph_from_uri (World: RDF.Rasqal.World.Handle_Type;
                                            URI, Name_URI: RDF.Raptor.URI.Handle_Type;
                                            Flags: Unsigned;
                                            Format_Type, Format_Name: Chars_Ptr;
                                            Format_URI: RDF.Raptor.URI.Handle_Type)
                                            return Data_Graph_Handle
     with Import, Convention=>C;

   function From_URI (World: RDF.Rasqal.World.World_Type_Without_Finalize'Class;
                      URI, Name_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class;
                      Flags: Flags_Type;
                      Format_Type, Format_Name: RDF.Auxiliary.String_Holders.Holder;
                      Format_URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class)
                      return Data_Graph_Type is
      use RDF.Auxiliary.C_String_Holders;
      Format_Type2: chars_ptr := New_String(Format_Type);
      Format_Name2: chars_ptr := New_String(Format_Name);
      use RDF.Rasqal.World, RDF.Raptor.IOStream, RDF.Raptor.URI;
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

end RDF.Rasqal.Data_Graph;
