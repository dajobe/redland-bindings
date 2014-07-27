with Ada.Unchecked_Conversion;
with RDF.Raptor.Constants;

package body RDF.Raptor.World is

   function C_Raptor_New_World_Internal(Version: Interfaces.C.unsigned) return RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type
     with Import, Convention=>C, External_Name=>"raptor_new_world_internal";

   procedure C_Raptor_World_Open(Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type)
     with Import, Convention=>C, External_Name=>"raptor_world_open";

   procedure C_Raptor_World_Set_Flag(Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type; Flag: Interfaces.C.int; Value: Interfaces.C.int)
     with Import, Convention=>C, External_Name=>"raptor_world_set_flag";

   function Default_Handle(Object: World_Type_Without_Finalize) return RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type is
   begin
      return C_Raptor_New_World_Internal(RDF.Raptor.Constants.Raptor_Version_Decimal);
   end;

   procedure Open(Object: World_Type_Without_Finalize) is
   begin
      C_Raptor_World_Open(Get_Handle(Object));
   end;

   procedure Open(Object: World_Type_Without_Finalize; Flags: Flags_Array) is
   begin
      Set_Flags(Object, Flags);
      Open(Object);
   end;

   function Open return World_Type_Without_Finalize is
      Object: World_Type_Without_Finalize;
   begin
      return Object: World_Type_Without_Finalize do
         Open(Object);
      end return;
   end;

   function Open(Flags: Flags_Array) return World_Type_Without_Finalize is
      Object: World_Type_Without_Finalize;
   begin
      return Object: World_Type_Without_Finalize do
         Open(Object, Flags);
      end return;
   end;

   function Flag_Conversion is new Ada.Unchecked_Conversion(Source=>Flag_Type, Target=>Interfaces.C.int);

   procedure Set_Flag(Object: World_Type_Without_Finalize; Flag: Flag_Type; Value: Boolean) is
   begin
      C_Raptor_World_Set_Flag(Get_Handle(Object), Flag_Conversion(Flag), (if Value then 1 else 0));
   end;

   procedure Set_Flags(Object: World_Type_Without_Finalize; Flags: Flags_Array) is
   begin
      for Element of Flags loop
         Set_Flag(Object, Element.Flag, Element.Value);
      end loop;
   end;

--   function From_Handle(Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type) return World is
--   begin
--      return (RDF.Base.From_Handle(Handle) with null record);
--   end From_Handle;

   procedure C_Raptor_Free_World(Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type)
     with Import, Convention=>C, External_Name=>"raptor_free_world";

   procedure Finalize_Handle(Object: World_Type; Handle: RDF.Auxilary.Simple_Limited_Handled_Record.Access_Type) is
   begin
      C_Raptor_Free_World(Handle);
   end;

end RDF.Raptor.World;
