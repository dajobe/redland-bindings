package body RDF.Raptor.WWW is

   function C_Raptor_New_WWW (World: RDF.Raptor.World.Handle_Type) return WWW_Handle_Type
      with Import, Convention=>C, External_Name=>"raptor_new_www";

   function New_WWW (World: RDF.Raptor.World.World_Type'Class) return WWW_Type is
      use RDF.Raptor.World;
   begin
      return From_Non_Null_Handle(C_Raptor_New_WWW(Get_Handle(World)));
   end;

   function C_Raptor_New_WWW_With_Connection (World: RDF.Raptor.World.Handle_Type; Connection: Connection_Type) return WWW_Handle_Type
      with Import, Convention=>C, External_Name=>"raptor_new_www_with_connection";

   function New_WWW (World: RDF.Raptor.World.World_Type'Class; Connection: Connection_Type) return WWW_Type is
      use RDF.Raptor.World;
   begin
      return From_Non_Null_Handle(C_Raptor_New_WWW_With_Connection(Get_Handle(World), Connection));
   end;

   procedure C_Raptor_Free_WWW (Handle: WWW_Handle_Type)
      with Import, Convention=>C, External_Name=>"raptor_free_www";

   procedure Finalize_Handle (Object: WWW_Type; Handle: WWW_Handle_Type) is
   begin
      C_Raptor_Free_WWW(Handle);
   end;

end RDF.Raptor.WWW;
