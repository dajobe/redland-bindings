with Ada.Unchecked_Conversion;

package body RDF.Raptor is

   function C_Raptor_New_World_Internal(Version: Interfaces.C.unsigned) return RDF.Base.Dummy_Record_Access
     with Import, Convention=>C, External_Name=>"raptor_new_world_internal";

   procedure C_Raptor_World_Open(Handle: RDF.Base.Dummy_Record_Access)
     with Import, Convention=>C, External_Name=>"raptor_world_open";

   procedure C_Raptor_Free_World(Handle: RDF.Base.Dummy_Record_Access)
     with Import, Convention=>C, External_Name=>"raptor_free_world";

   procedure C_Raptor_World_Set_Flag(Handle: RDF.Base.Dummy_Record_Access; Flag: Interfaces.C.int; Value: Interfaces.C.int)
     with Import, Convention=>C, External_Name=>"raptor_world_set_flag";

   function Default_Handle(Object: World) return RDF.Base.Dummy_Record_Access is
   begin
      return C_Raptor_New_World_Internal(Raptor_Version_Decimal);
   end;

   procedure Open(Object: World) is
   begin
      C_Raptor_World_Open(Get_Handle(Object));
   end;

   procedure Open(Object: World; Flags: Flags_Array) is
   begin
      Set_Flags(Object, Flags);
      Open(Object);
   end;

   function Open return World is
      Object: World;
   begin
      return Object: World do
         Open(Object);
      end return;
   end;

   function Open(Flags: Flags_Array) return World is
      Object: World;
   begin
      return Object: World do
         Open(Object, Flags);
      end return;
   end;

   procedure Finalize_Handle(Object: World; Handle: RDF.Base.Dummy_Record_Access) is
   begin
      C_Raptor_Free_World(Handle);
   end;

   function Flag_Conversion is new Ada.Unchecked_Conversion(Source=>Flag_Type, Target=>Interfaces.C.int);

   procedure Set_Flag(Object: World; Flag: Flag_Type; Value: Boolean) is
   begin
      C_Raptor_World_Set_Flag(Get_Handle(Object), Flag_Conversion(Flag), (if Value then 1 else 0));
   end;

   procedure Set_Flags(Object: World; Flags: Flags_Array) is
   begin
      for Element of Flags loop
         Set_Flag(Object, Element.Flag, Element.Value);
      end loop;
   end;

--   function From_Handle(Handle: RDF.Base.Dummy_Record_Access) return World is
--   begin
--      return (RDF.Base.From_Handle(Handle) with null record);
--   end From_Handle;

end RDF.Raptor;
