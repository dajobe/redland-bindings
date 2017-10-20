with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.Convert_Void;
with RDF.Auxiliary.C_String_Holders; use RDF.Auxiliary.C_String_Holders;
with RDF.Rasqal.Memory;

package body RDF.Rasqal.World is

   function rasqal_new_world return Rasqal_World_Handle
     with Import, Convention=>C;

   procedure rasqal_world_open(Handle: Rasqal_World_Handle)
     with Import, Convention=>C;

   function Default_Handle(Object: Rasqal_World_Type_Without_Finalize)
                           return Rasqal_World_Handle is
   begin
      return rasqal_new_world;
   end;

   procedure Open(Object: in out Rasqal_World_Type_Without_Finalize) is
   begin
      rasqal_world_open(Get_Handle(Object));
   end;

   function Open return Rasqal_World_Type is
   begin
      return Object: Rasqal_World_Type do
         Open(Object);
      end return;
   end;

   function rasqal_world_set_warning_level (World: Rasqal_World_Handle; Level: unsigned)
                                            return int
     with Import, Convention=>C;

   procedure Set_Warning_Level (World: in out Rasqal_World_Type_Without_Finalize;
                                Level: Warning_Level) is
   begin
      if rasqal_world_set_warning_level(Get_Handle(World), unsigned(Level)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function rasqal_world_get_raptor (World: Rasqal_World_Handle) return Raptor_World_Handle
     with Import, Convention=>C;

   function Get_Raptor (World: Rasqal_World_Type_Without_Finalize)
                        return Raptor_World_Type_Without_Finalize is
   begin
      return From_Non_Null_Handle(rasqal_world_get_raptor(Get_Handle(World)));
   end;

   procedure rasqal_world_set_raptor (World: Rasqal_World_Handle; Raptor_World: Raptor_World_Handle)
     with Import, Convention=>C;

   procedure Set_Raptor (World: in out Rasqal_World_Type_Without_Finalize;
                         Raptor_World: Raptor_World_Type_Without_Finalize'Class) is
   begin
      rasqal_world_set_raptor(Get_Handle(World), Get_Handle(Raptor_World));
   end;

   procedure rasqal_free_world (World: Rasqal_World_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Rasqal_World_Type_Without_Finalize; Handle: Rasqal_World_Handle) is
   begin
      rasqal_free_world(Handle);
   end;

--     type Log_Handler_Access is access constant Log_Handler'Class;
   package My_Conv is new RDF.Auxiliary.Convert_Void(Log_Handler'Class);

   procedure rasqal_world_set_log_handler (World: Rasqal_World_Handle;
                                           Data: chars_ptr;
                                           Handler: Log_Handler_Procedure_Type)
     with Import, Convention=>C;

   procedure Set_Log_Handler(World: in out Rasqal_World_Type_Without_Finalize;
                             Handler: access Log_Handler'Class) is
   begin
      rasqal_world_set_log_handler(Get_Handle(World),
                                   My_Conv.To_C_Pointer(My_Conv.Object_Pointer(Handler)),
                                   Our_Raptor_Log_Handler'Access);
   end;

   function rasqal_world_guess_query_results_format_name (World: Rasqal_World_Handle;
                                                          URI: URI_Handle;
                                                          Mime_Type: chars_ptr;
                                                          Buffer: chars_ptr;
                                                          Len: size_t;
                                                          Identifier: chars_ptr)
                                                          return chars_ptr
     with Import, Convention=>C;

   function Guess_Query_Results_Format_Name (World: Rasqal_World_Type_Without_Finalize;
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
