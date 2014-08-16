with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with RDF.Raptor.Constants;
with RDF.Raptor.Memory;
with RDF.Auxilary.C_String_Holders; use RDF.Auxilary.C_String_Holders;

package body RDF.Raptor.World is

   function C_Raptor_New_World_Internal(Version: Interfaces.C.unsigned) return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_world_internal";

   procedure C_Raptor_World_Open(Handle: Handle_Type)
     with Import, Convention=>C, External_Name=>"raptor_world_open";

   procedure C_Raptor_World_Set_Flag(Handle: Handle_Type; Flag: Interfaces.C.int; Value: Interfaces.C.int)
     with Import, Convention=>C, External_Name=>"raptor_world_set_flag";

   function Default_Handle(Object: World_Type_Without_Finalize) return Handle_Type is
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

   procedure C_Raptor_Free_World(Handle: Handle_Type)
     with Import, Convention=>C, External_Name=>"raptor_free_world";

   procedure Finalize_Handle(Object: World_Type; Handle: Handle_Type) is
   begin
      C_Raptor_Free_World(Handle);
   end;

   function C_Raptor_World_Generate_Bnodeid (World: Handle_Type) return chars_ptr
     with Import, Convention=>C, External_Name=>"raptor_world_generate_bnodeid";

   function Generate_Bnodeid (World: World_Type_Without_Finalize) return String is
      C_Str: chars_ptr := C_Raptor_World_Generate_Bnodeid(Get_Handle(World));
      Result: constant String := Value(C_Str);
   begin
      RDF.Raptor.Memory.raptor_free_memory(C_Str);
      return Result;
   end;

   procedure C_Raptor_World_Set_Generate_Bnodeid_Parameters (World: Handle_Type; Prefix: chars_ptr; Base: int)
     with Import, Convention=>C, External_Name=>"raptor_world_set_generate_bnodeid_parameters";

   procedure Set_Generate_Bnodeid_Parameters (World: World_Type_Without_Finalize;
                                              Prefix: String_Holders.Holder;
                                              Base: int) is
      Prefix_N : C_String_Holder := To_C_String_Holder(Prefix);
   begin
      C_Raptor_World_Set_Generate_Bnodeid_Parameters(Get_Handle(World), C_String(Prefix_N), Base);
   end;

end RDF.Raptor.World;
