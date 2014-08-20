with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.World;
with RDF.Auxilary;

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

   type Locator_Type is private;

   type Log_Message_Type is private;

   function Get_URI (Locator: Locator_Type) return URI_Type_Without_Finalize;

   function Get_File (Locator: Locator_Type) return String;

   function Get_Line   (Locator: Locator_Type) return Natural;
   function Get_Column (Locator: Locator_Type) return Natural;
   function Get_Byte   (Locator: Locator_Type) return Natural;

   procedure Print (Locator: Locator_Type; File: RDF.Auxilary.C_File_Access);

   function Format (Locator: Locator_Type) return String;

   function Get_Error_Code (Message: Log_Message_Type) return int;
   function Get_Domain (Message: Log_Message_Type) return Domain_Type;
   function Get_Log_Level (Message: Log_Message_Type) return Log_Level_Type;
   function Get_Text (Message: Log_Message_Type) return String;
   function Get_Locator (Message: Log_Message_Type) return Locator_Type;

   type Log_Handler is abstract tagged null record;

   procedure Log_Message(Handler: Log_Handler; Info: Log_Message_Type) is abstract;

   procedure Set_Log_Handler(World: RDF.Raptor.World.World_Type_Without_Finalize'Class; Handler: in out Log_Handler);

   function Get_Label (Level: Log_Level_Type) return String;
   function Get_Label (Level: Domain_Type) return String;

private

   type Locator_Type is
      record
         URI: RDF.Raptor.URI.Handle_Type;
         File: chars_ptr;
         Line, Column, Byte: int;
      end record
      with Convention => C;

   type Log_Message_Type is
      record
         Code: Interfaces.C.int;
         Domain: Domain_Type;
         Log_Level: Log_Level_Type;
         Locator: access Locator_Type;
         Text: chars_ptr;
      end record
      with Convention => C;

end RDF.Raptor.Log;
