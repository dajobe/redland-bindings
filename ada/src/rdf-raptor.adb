package body RDF.Raptor is

   function C_Raptor_New_World return RDF.Base.Dummy_Record_Access
     with Import, Convention=>C, External_Name=>"raptor_new_world";

   procedure C_Raptor_Free_World(Handle: RDF.Base.Dummy_Record_Access)
     with Import, Convention=>C, External_Name=>"raptor_free_world";

   function Default_Handle(Object: World) return RDF.Base.Dummy_Record_Access is
   begin
      return C_Raptor_New_World;
   end;

   procedure Finalize_Handle(Object: World; Handle: RDF.Base.Dummy_Record_Access) is
   begin
      C_Raptor_Free_World(Handle);
   end;

   function From_Handle(Handle: RDF.Base.Dummy_Record_Access) return World is
   begin
      return (RDF.Base.From_Handle(Handle) with null record);
   end From_Handle;

end RDF.Raptor;
