with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary;
with RDF.Raptor.Log;
with RDF.Redland.World; use RDF.Redland.World;

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
--     package Log_Message_Handled_Record is new RDF.Auxiliary.Handled_Record(Log_Message_Record, Log_Message_Record_Access);
--     subtype Log_Message_Handle is Log_Message_Handled_Record.Access_Type;
--     type Log_Message_Type is new Log_Message_Handled_Record.Base_Object with null record;

   function Get_Code (Message: Log_Message_Type) return int;
   function Get_Level (Message: Log_Message_Type) return Log_Level_Type;
   function Get_Facility (Message: Log_Message_Type) return Log_Facility_Type;
   function Get_Message (Message: Log_Message_Type) return String;
   function Get_Locator (Message: Log_Message_Type) return RDF.Raptor.Log.Locator_Type;

   type Log_Handler is abstract tagged null record;

   type Log_Handler_Procedure_Type is access procedure (Data: chars_ptr; Msg: Log_Message_Type)
     with Convention=>C;

   -- Internal
   procedure Our_Raptor_Log_Handler(Data: chars_ptr; Msg: Log_Message_Type)
     with Convention=>C;

   not overriding procedure Log_Message(Handler: Log_Handler; Info: Log_Message_Type) is abstract;

   not overriding procedure Set_Log_Handler(World: in out Redland_World_Type_Without_Finalize'Class;
                                            Handler: access Log_Handler);

private

   type Log_Message_Type is new RDF.Auxiliary.Dummy_Record_Access;

end RDF.Redland.Log;
