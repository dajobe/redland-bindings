with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary;
with RDF.Auxiliary.C_String_Holders; use RDF.Auxiliary.C_String_Holders;
with RDF.Raptor.URI;
with RDF.Rasqal.Memory;

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

   procedure Set_Log_Handler(World: World_Type_Without_Finalize; Handler: access RDF.Raptor.Log.Log_Handler) is
   begin
      rasqal_world_set_log_handler(Get_Handle(World), Obj_To_Ptr(Handler), RDF.Raptor.Log.Our_Raptor_Log_Handler'Access);
   end;

   function rasqal_world_guess_query_results_format_name (World: RDF.Rasqal.World.Handle_Type;
                                                          URI: RDF.Raptor.URI.Handle_Type;
                                                          Mime_Type: chars_ptr;
                                                          Buffer: chars_ptr;
                                                          Len: size_t;
                                                          Identifier: chars_ptr)
                                                          return chars_ptr
         with Import, Convention=>C;

   -- FIXME: Check if need to deallocate the result of the C function
   function Rasqal_World_Guess_Query_Results_Format_Name (World: World_Type_Without_Finalize;
                                                          URI: URI_Type_Without_Finalize'Class;
                                                          Mime_Type: String_Holders.Holder;
                                                          Buffer: String_Holders.Holder;
                                                          Identifier: String_Holders.Holder)
                                                          return String_Holders.Holder is
      Buffer2: constant C_String_Holder := To_C_String_Holder(Buffer);
      Mime_Type2 : constant chars_ptr := New_String(Mime_Type );
      Identifier2: constant chars_ptr := New_String(Identifier);
      Result: constant chars_ptr := rasqal_world_guess_query_results_format_name(Get_Handle(World),
                                                                                 Get_Handle(URI),
                                                                                 Mime_Type2,
                                                                                 C_String(Buffer2),
                                                                                 Length(Buffer2),
                                                                                 Identifier2);
      use String_Holders;
   begin
      RDF.Rasqal.Memory.rasqal_free_memory(Mime_Type2);
      RDF.Rasqal.Memory.rasqal_free_memory(Identifier2);
      return (if Result = Null_Ptr then Empty_Holder else To_Holder(Value(Result)));
   end;

end RDF.Rasqal.World;
