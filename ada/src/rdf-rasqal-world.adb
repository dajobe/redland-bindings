with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary;

package body RDF.Rasqal.World is

   function rasqal_new_world return Handle_Type
     with Import, Convention=>C;

   procedure rasqal_world_open(Handle: Handle_Type)
     with Import, Convention=>C;

   function Default_Handle(Object: World_Type_Without_Finalize) return Handle_Type is
   begin
      return rasqal_new_world;
   end;

   procedure Open(Object: World_Type_Without_Finalize) is
   begin
      rasqal_world_open(Get_Handle(Object));
   end;

   function Open return World_Type is
      Object: World_Type;
   begin
      return Object: World_Type do
         Open(Object);
      end return;
   end;

   function rasqal_world_set_warning_level (World: Handle_Type; Level: unsigned) return int
      with Import, Convention=>C;

   procedure Set_Warning_Level (World: World_Type_Without_Finalize; Level: Warning_Level) is
   begin
      if rasqal_world_set_warning_level(Get_Handle(World), unsigned(Level)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function rasqal_world_get_raptor (World: Handle_Type) return RDF.Raptor.World.Handle_Type
      with Import, Convention=>C;

   function Get_Raptor (World: World_Type_Without_Finalize) return RDF.Raptor.World.World_Type_Without_Finalize is
      use RDF.Raptor.World;
   begin
      return From_Non_Null_Handle(rasqal_world_get_raptor(Get_Handle(World)));
   end;

   procedure rasqal_world_set_raptor (World: Handle_Type; Raptor_World: RDF.Raptor.World.Handle_Type)
      with Import, Convention=>C;

   procedure Set_Raptor (World: World_Type_Without_Finalize; Raptor_World: RDF.Raptor.World.World_Type_Without_Finalize) is
      use RDF.Raptor.World;
   begin
      rasqal_world_set_raptor(Get_Handle(World), Get_Handle(Raptor_World));
   end;

   procedure rasqal_free_world (World: Handle_Type)
      with Import, Convention=>C;

   procedure Finalize_Handle (Object: World_Type; Handle: Handle_Type) is
   begin
      rasqal_free_world(Handle);
   end;

   type Log_Handler_Access is access constant RDF.Raptor.Log.Log_Handler'Class;
   function Ptr_To_Obj is new Ada.Unchecked_Conversion(chars_ptr, Log_Handler_Access);
   function Obj_To_Ptr is new Ada.Unchecked_Conversion(Log_Handler_Access, chars_ptr);

   procedure rasqal_world_set_log_handler (World: Handle_Type; Data: chars_ptr; Handler: RDF.Raptor.Log.Log_Handler_Procedure_Type)
      with Import, Convention=>C;

   procedure Set_Log_Handler(World: World_Type_Without_Finalize; Handler: RDF.Raptor.Log.Log_Handler) is
   begin
      rasqal_world_set_log_handler(Get_Handle(World), Obj_To_Ptr(Handler'Unchecked_Access), RDF.Raptor.Log.Our_Raptor_Log_Handler'Access);
   end;

end RDF.Rasqal.World;
