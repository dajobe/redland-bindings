with RDF.Auxiliary.Convert_Void;

package body RDF.Redland.Log is

   function librdf_log_message_code (Message: Log_Message_Type) return int
     with Import, Convention=>C;

   function Get_Code (Message: Log_Message_Type) return int is
   begin
      return librdf_log_message_code(Message);
   end;

   function librdf_log_message_level (Message: Log_Message_Type) return Log_Level_Type
     with Import, Convention=>C;

   function Get_Level (Message: Log_Message_Type) return Log_Level_Type is
   begin
      return librdf_log_message_level(Message);
   end;

   function librdf_log_message_facility (Message: Log_Message_Type) return Log_Facility_Type
     with Import, Convention=>C;

   function Get_Facility (Message: Log_Message_Type) return Log_Facility_Type is
   begin
      return librdf_log_message_facility(Message);
   end;

   function librdf_log_message_message (Message: Log_Message_Type) return chars_ptr
     with Import, Convention=>C;

   function Get_Message (Message: Log_Message_Type) return String is
   begin
      return Value(librdf_log_message_message(Message));
   end;

   function librdf_log_message_locator (Message: Log_Message_Type) return RDF.Raptor.Log.Locator_Handle
     with Import, Convention=>C;

   function Get_Locator (Message: Log_Message_Type) return RDF.Raptor.Log.Locator_Type is
      use RDF.Raptor.Log;
   begin
      return From_Handle(librdf_log_message_locator(Message));
   end;

--     type User_Defined_Access is access constant Log_Handler'Class;
   package My_Conv is new RDF.Auxiliary.Convert_Void(Log_Handler'Class);

   procedure Our_Raptor_Log_Handler(Data: chars_ptr; Msg: Log_Message_Type) is
   begin
      Log_Message(My_Conv.To_Access(Data).all, Msg);
   end;

   procedure librdf_world_set_logger (World: Redland_World_Handle; Data: chars_ptr; Handler: Log_Handler_Procedure_Type)
     with Import, Convention=>C;

   procedure Set_Log_Handler(World: in out Redland_World_Type_Without_Finalize'Class; Handler: access Log_Handler) is
   begin
      librdf_world_set_logger(Get_Handle(World), My_Conv.To_C_Pointer(Handler), Our_Raptor_Log_Handler'Access);
   end;

end RDF.Redland.Log;
