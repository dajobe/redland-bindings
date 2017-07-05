package body RDF.Raptor.WWW is

   function C_Raptor_New_WWW (World: RDF.Raptor.World.Handle_Type) return WWW_Handle_Type
      with Import, Convention=>C, External_Name=>"raptor_new_www";

   function New_WWW (World: RDF.Raptor.World.World_Type'Class) return WWW_Type is
      use RDF.Raptor.World;
   begin
      return From_Non_Null_Handle(C_Raptor_New_WWW(Get_Handle(World)));
   end;

end RDF.Raptor.WWW;
