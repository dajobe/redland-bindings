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

end RDF.Redland.Log;
