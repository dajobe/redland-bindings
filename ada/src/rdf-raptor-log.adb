with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Auxilary;

package body RDF.Raptor.Log is

   function Get_URI (Locator: Locator_Type) return URI_Type_Without_Finalize is
   begin
      return From_Handle(Locator.URI);
   end;

   function Get_File (Locator: Locator_Type) return String is
   begin
      return Value(Locator.File);
   end;

   function Get_Line (Locator: Locator_Type) return Natural is
   begin
      return Integer(Locator.Line);
   end;

   function Get_Column (Locator: Locator_Type) return Natural is
   begin
      return Integer(Locator.Column);
   end;

   function Get_Byte (Locator: Locator_Type) return Natural is
   begin
      return Integer(Locator.Byte);
   end;

   function Get_Error_Code (Message: Log_Message_Type) return int is
   begin
      return Message.Code;
   end;

   function Get_Domain (Message: Log_Message_Type) return Domain_Type is
   begin
      return Message.Domain;
   end;

   function Get_Log_Level (Message: Log_Message_Type) return Log_Level_Type is
   begin
      return Message.Log_Level;
   end;

   function Get_Text (Message: Log_Message_Type) return String is
   begin
      return Value(Message.Text);
   end;

   function Get_Locator (Message: Log_Message_Type) return Locator_Type is
   begin
      return Message.Locator.all;
   end;

   type Log_Handler_Type is access procedure (Data: chars_ptr; Msg: Log_Message_Type)
      with Convention=>C;

   type User_Defined_Access is access all Log_Handler'Class;
   function Ptr_To_Obj is new Ada.Unchecked_Conversion(chars_ptr, User_Defined_Access);
   function Obj_To_Ptr is new Ada.Unchecked_Conversion(User_Defined_Access, chars_ptr);

   procedure C_Raptor_Log_Handler(Data: chars_ptr; Msg: Log_Message_Type)
      with Convention=>C;

   procedure C_Raptor_Log_Handler(Data: chars_ptr; Msg: Log_Message_Type) is
   begin
      Log_Message(Ptr_To_Obj(Data).all, Msg);
   end;

   function C_Raptor_World_Set_Log_Handler(World: RDF.Raptor.World.Handle_Type; Data: chars_ptr; Handler: Log_Handler_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_world_set_log_handler";

   procedure Set_Log_Handler(World: RDF.Raptor.World.World_Type_Without_Finalize'Class; Handler: in out Log_Handler) is
   begin
      if C_Raptor_World_Set_Log_Handler(Get_Handle(World), Obj_To_Ptr(Handler'Unchecked_Access), C_Raptor_Log_Handler'Access) /= 0 then
         raise RDF.Auxilary.RDF_Exception;
      end if;
   end;

   function C_Raptor_Log_Level_Get_Label(Level: Log_Level_Type) return chars_ptr
     with Import, Convention=>C, External_Name=>"raptor_log_level_get_label";

   function Get_Label (Level: Log_Level_Type) return String is
   begin
      return Value(C_Raptor_Log_Level_Get_Label(Level));
   end;

   function C_Raptor_Domain_Get_Label(Level: Domain_Type) return chars_ptr
     with Import, Convention=>C, External_Name=>"raptor_domain_get_label";

   function Get_Label (Level: Domain_Type) return String is
   begin
      return Value(C_Raptor_Domain_Get_Label(Level));
   end;

end RDF.Raptor.Log;
