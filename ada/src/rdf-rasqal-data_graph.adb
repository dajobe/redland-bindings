package body RDF.Rasqal.Data_Graph is

   -- FIXME: Rewrite

   ---------------
   -- Get_World --
   ---------------

   function Get_World
     (Graph: Data_Graph_Type_Without_Finalize)
      return RDF.Rasqal.World.World_Type_Without_Finalize
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_World unimplemented");
      raise Program_Error with "Unimplemented function Get_World";
      return Get_World (Graph => Graph);
   end Get_World;

   -------------
   -- Get_URI --
   -------------

   function Get_URI
     (Graph: Data_Graph_Type_Without_Finalize)
      return RDF.Raptor.URI.URI_Type_Without_Finalize
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_URI unimplemented");
      raise Program_Error with "Unimplemented function Get_URI";
      return Get_URI (Graph => Graph);
   end Get_URI;

   ------------------
   -- Get_Name_URI --
   ------------------

   function Get_Name_URI
     (Graph: Data_Graph_Type_Without_Finalize)
      return RDF.Raptor.URI.URI_Type_Without_Finalize
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Name_URI unimplemented");
      raise Program_Error with "Unimplemented function Get_Name_URI";
      return Get_Name_URI (Graph => Graph);
   end Get_Name_URI;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags
     (Graph: Data_Graph_Type_Without_Finalize)
      return Flags_Type
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Flags unimplemented");
      raise Program_Error with "Unimplemented function Get_Flags";
      return Get_Flags (Graph => Graph);
   end Get_Flags;

   ---------------------
   -- Get_Format_Type --
   ---------------------

   function Get_Format_Type
     (Graph: Data_Graph_Type_Without_Finalize)
      return String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Format_Type unimplemented");
      raise Program_Error with "Unimplemented function Get_Format_Type";
      return Get_Format_Type (Graph => Graph);
   end Get_Format_Type;

   ---------------------
   -- Get_Format_Name --
   ---------------------

   function Get_Format_Name
     (Graph: Data_Graph_Type_Without_Finalize)
      return String
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Format_Name unimplemented");
      raise Program_Error with "Unimplemented function Get_Format_Name";
      return Get_Format_Name (Graph => Graph);
   end Get_Format_Name;

   --------------------
   -- Get_Format_URI --
   --------------------

   function Get_Format_URI
     (Graph: Data_Graph_Type_Without_Finalize)
      return RDF.Raptor.URI.URI_Type
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Format_URI unimplemented");
      raise Program_Error with "Unimplemented function Get_Format_URI";
      return Get_Format_URI (Graph => Graph);
   end Get_Format_URI;

   ------------------
   -- Get_Iostream --
   ------------------

   function Get_Iostream
     (Graph: Data_Graph_Type_Without_Finalize)
      return RDF.Raptor.IOStream.Stream_Type_Without_Finalize
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Iostream unimplemented");
      raise Program_Error with "Unimplemented function Get_Iostream";
      return Get_Iostream (Graph => Graph);
   end Get_Iostream;

   ------------------
   -- Get_Base_URI --
   ------------------

   function Get_Base_URI
     (Graph: Data_Graph_Type_Without_Finalize)
      return RDF.Raptor.URI.URI_Type
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Base_URI unimplemented");
      raise Program_Error with "Unimplemented function Get_Base_URI";
      return Get_Base_URI (Graph => Graph);
   end Get_Base_URI;

   ---------------------
   -- Get_Usage_Count --
   ---------------------

   function Get_Usage_Count
     (Graph: Data_Graph_Type_Without_Finalize)
      return Natural
   is
   begin
      --  Generated stub: replace with real body!
      pragma Compile_Time_Warning (Standard.True, "Get_Usage_Count unimplemented");
      raise Program_Error with "Unimplemented function Get_Usage_Count";
      return Get_Usage_Count (Graph => Graph);
   end Get_Usage_Count;

end RDF.Rasqal.Data_Graph;
