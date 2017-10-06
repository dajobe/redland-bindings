with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.Handled_Record;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Auxiliary;

package RDF.Raptor.Log is

   type Log_Level_Type is (None, Trace, Debug, Info, Warn, Error, Fatal)
     with Convention => C;
   for Log_Level_Type use (None => 0,
                           Trace => 1,
                           Debug => 2,
                           Info => 3,
                           Warn => 4,
                           Error => 5,
                           Fatal => 6);
   function Last return Log_Level_Type renames Fatal;

   type Locator_Type_Record is private;
   type Log_Message_Record  is private;

   type Locator_Type_Record_Access is access Locator_Type_Record with Convention=>C;
   type Log_Message_Record_Access is access Log_Message_Record with Convention=>C;

   package Locator_Handled_Record is new RDF.Auxiliary.Handled_Record(Locator_Type_Record, Locator_Type_Record_Access);
   subtype Locator_Handle is Locator_Handled_Record.Access_Type;
   type Locator_Type is new Locator_Handled_Record.Base_Object with null record;

   overriding procedure Finalize_Handle (Object: Locator_Type; Handle: Locator_Handle);
   overriding procedure Adjust (Object: in out Locator_Type);

   package Log_Message_Handled_Record is new RDF.Auxiliary.Handled_Record(Log_Message_Record, Log_Message_Record_Access);
   subtype Log_Message_Handle is Log_Message_Handled_Record.Access_Type;
   type Log_Message_Type is new Log_Message_Handled_Record.Base_Object with null record;

   overriding procedure Finalize_Handle (Object: Log_Message_Type; Handle: Log_Message_Handle);
   overriding procedure Adjust (Object: in out Log_Message_Type);

   not overriding function Get_URI (Locator: Locator_Type) return URI_Type_Without_Finalize;

   not overriding function Get_File (Locator: Locator_Type) return String;

   not overriding function Get_Line   (Locator: Locator_Type) return Natural;
   not overriding function Get_Column (Locator: Locator_Type) return Natural;
   not overriding function Get_Byte   (Locator: Locator_Type) return Natural;

   not overriding procedure Print (Locator: Locator_Type; File: RDF.Auxiliary.C_File_Access);

   not overriding function Format (Locator: Locator_Type) return String;

   not overriding function Get_Error_Code (Message: Log_Message_Type) return int;
   not overriding function Get_Domain (Message: Log_Message_Type) return Domain_Type;
   not overriding function Get_Log_Level (Message: Log_Message_Type) return Log_Level_Type;
   not overriding function Get_Text (Message: Log_Message_Type) return String;
   not overriding function Get_Locator (Message: Log_Message_Type'Class) return Locator_Type;

   type Log_Handler is abstract tagged null record;

   type Log_Handler_Procedure_Type is access procedure (Data: Chars_Ptr; Msg: Log_Message_Type)
     with Convention=>C;

   -- Internal
   procedure Our_Raptor_Log_Handler(Data: chars_ptr; Msg: Log_Message_Type)
     with Convention=>C;

   not overriding procedure Log_Message(Handler: Log_Handler; Info: Log_Message_Type'Class) is abstract;

   not overriding procedure Set_Log_Handler(World: in out Raptor_World_Type_Without_Finalize'Class;
                                            Handler: access Log_Handler);

   not overriding function Get_Label (Level: Log_Level_Type) return String;
   function Get_Label (Level: Domain_Type) return String;

private

   type Locator_Type_Record is
      record
         URI: URI_Handle;
         File: chars_ptr;
         Line, Column, Byte: int;
      end record
     with Convention => C;

   type Log_Message_Record is
      record
         Code: Interfaces.C.int;
         Domain: Domain_Type;
         Log_Level: Log_Level_Type;
         --           Locator: access Locator_Type;
         Locator: Locator_Handle;
         Text: chars_ptr;
      end record
     with Convention => C;

end RDF.Raptor.Log;
