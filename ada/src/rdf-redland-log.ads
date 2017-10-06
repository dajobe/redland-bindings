with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Raptor.Log;
with RDF.Auxiliary;

package RDF.Redland.Log is

   type Log_Level_Type is (None, Debug, Info, Warn, Error, Fatal)
     with Convention => C;
   for Log_Level_Type use (None => 0,
                           Debug => 1,
                           Info => 2,
                           Warn => 3,
                           Error => 4,
                           Fatal => 5);
   function Last return Log_Level_Type renames Fatal;

   type Log_Facility_Type is (None,
                              Concepts,
                              Digest,
                              Files,
                              Hash,
                              Init,
                              Iterator,
                              List,
                              Model,
                              Node,
                              Parser,
                              Query,
                              Serializer,
                              Statement,
                              Storage,
                              Stream,
                              URI,
                              UTF8,
                              Memory,
                              Raptor)
     with Convention => C;
   function Last return Log_Facility_Type renames Raptor;

   type Log_Message_Type is private;

   function Get_Code (Message: Log_Message_Type) return int;
   function Get_Level (Message: Log_Message_Type) return Log_Level_Type;
   function Get_Facility (Message: Log_Message_Type) return Log_Facility_Type;
   function Get_Message (Message: Log_Message_Type) return String;
   function Get_Locator (Message: Log_Message_Type) return RDF.Raptor.Log.Locator_Type;

   type Log_Handler is abstract tagged null record;

   type Log_Handler_Procedure_Type is access procedure (Data: chars_ptr; Msg: Log_Message_Type)
     with Convention=>C;

   -- Internal
   -- FIXME: Uncomment
--     procedure Our_Raptor_Log_Handler(Data: chars_ptr; Msg: Log_Message_Type)
--       with Convention=>C;

   -- TODO: Stopped at librdf_log_message_code()

private

   type Log_Message_Type is new RDF.Auxiliary.Dummy_Record_Access;

end RDF.Redland.Log;
