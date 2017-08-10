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

   ------------------
   -- Get_Base_URI --
   ------------------

   function Get_Base_URI (Graph: Data_Graph_Type_Without_Finalize) return RDF.Raptor.URI.URI_Type is
      use RDF.Raptor.URI;
   begin
      return From_Handle(Get_Handle(Graph).Base_URI);
   end;

   ---------------------
   -- Get_Usage_Count --
   ---------------------

   function Get_Usage_Count (Graph: Data_Graph_Type_Without_Finalize) return Natural is
      (Natural(Get_Handle(Graph).Usage));

end RDF.Rasqal.Data_Graph;
