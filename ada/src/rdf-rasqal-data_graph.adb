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

end RDF.Rasqal.Data_Graph;
