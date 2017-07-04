with Ada.Unchecked_Conversion;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Raptor.Memory;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Auxiliary;

package body RDF.Raptor.Log is

   function Get_URI (Locator: Locator_Type) return URI_Type_Without_Finalize is
   begin
      return From_Handle(Get_Handle(Locator).URI); -- FIXME
   end;

   function Get_File (Locator: Locator_Type) return String is
   begin
      return Value(Get_Handle(Locator).File);
   end;

   function Get_Line (Locator: Locator_Type) return Natural is
   begin
      return Integer(Get_Handle(Locator).Line);
   end;

   function Get_Column (Locator: Locator_Type) return Natural is
   begin
      return Integer(Get_Handle(Locator).Column);
   end;

   function Get_Byte (Locator: Locator_Type) return Natural is
   begin
      return Integer(Get_Handle(Locator).Byte);
   end;

   function Get_Error_Code (Message: Log_Message_Type) return int is
   begin
      return Get_Handle(Message).Code;
   end;

   function Get_Domain (Message: Log_Message_Type) return Domain_Type is
   begin
      return Get_Handle(Message).Domain;
   end;

   function Get_Log_Level (Message: Log_Message_Type) return Log_Level_Type is
   begin
      return Get_Handle(Message).Log_Level;
   end;

   function Get_Text (Message: Log_Message_Type) return String is
   begin
      return Value(Get_Handle(Message).Text);
   end;

   function Get_Locator (Message: Log_Message_Type'Class) return Locator_Type is
   begin
      return From_Non_Null_Handle(Get_Handle(Message).Locator);
   end;

   type Log_Handler_Type is access procedure (Data: chars_ptr; Msg: Log_Message_Type)
      with Convention=>C;

   type User_Defined_Access is access constant Log_Handler'Class;
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

   procedure Set_Log_Handler(World: RDF.Raptor.World.World_Type_Without_Finalize'Class; Handler: Log_Handler) is
   begin
      if C_Raptor_World_Set_Log_Handler(Get_Handle(World), Obj_To_Ptr(Handler'Unchecked_Access), C_Raptor_Log_Handler'Access) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function C_Raptor_Log_Level_Get_Label(Level: Log_Level_Type) return chars_ptr
     with Import, Convention=>C, External_Name=>"raptor_log_level_get_label";

   function Get_Label (Level: Log_Level_Type) return String is
   begin
      return Value(C_Raptor_Log_Level_Get_Label(Level));
   end;

   function C_Raptor_Domain_Get_Label (Level: Domain_Type) return chars_ptr
     with Import, Convention=>C, External_Name=>"raptor_domain_get_label";

   function Get_Label (Level: Domain_Type) return String is
   begin
      return Value(C_Raptor_Domain_Get_Label(Level));
   end;

   function C_Raptor_Locator_Print (Locator: Locator_Handle_Type; Stream: RDF.Auxiliary.C_File_Access) return int
     with Import, Convention=>C, External_Name=>"raptor_locator_print";

   procedure Print (Locator: Locator_Type; File: RDF.Auxiliary.C_File_Access) is
   begin
      if C_Raptor_Locator_Print(Get_Handle(Locator), File) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function C_Raptor_Locator_Format (Buffer: chars_ptr; Length: size_t; Locator: Locator_Handle_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_locator_format";

   function Format (Locator: Locator_Type) return String is
      Res1: constant int := C_Raptor_Locator_Format(Null_Ptr, 0, Get_Handle(Locator));
   begin
      if Res1 < 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      declare
         Buffer: aliased char_array := (1..size_t(Res1) => Interfaces.C.Nul);
      begin
         if C_Raptor_Locator_Format(To_Chars_Ptr(Buffer'Unchecked_Access, Nul_Check=>False), 0, Get_Handle(Locator)) < 0 then
            raise RDF.Auxiliary.RDF_Exception;
         end if;
         return To_Ada(Buffer);
      end;
   end;

   procedure C_Raptor_Free_URI (Handle: RDF.Raptor.URI.Handle_Type)
     with Import, Convention=>C, External_Name=>"raptor_free_uri";

   function C_Raptor_URI_Copy (Handle: RDF.Raptor.URI.Handle_Type)
                               return RDF.Raptor.URI.Handle_Type
   with Import, Convention=>C, External_Name=>"raptor_uri_copy";

   procedure Finalize_Locator (Handle: Locator_Handle_Type) is
      function Conv is new Ada.Unchecked_Conversion(Locator_Handle_Type, chars_ptr);
   begin
      C_Raptor_Free_URI(Handle.URI);
      RDF.Raptor.Memory.Raptor_Free_Memory(Handle.File);
      RDF.Raptor.Memory.Raptor_Free_Memory(Conv(Handle));
   end;

   procedure Finalize_Handle (Object: Locator_Type; Handle: Locator_Handle_Type) is
   begin
      Finalize_Locator(Handle);
   end;

   procedure Adjust (Object: in out Locator_Type) is
   begin
      Get_Handle(Object).URI := C_Raptor_URI_Copy(Get_Handle(Object).URI);
      Get_Handle(Object).File := RDF.Raptor.Memory.Copy_C_String(Get_Handle(Object).File);
   end;

   procedure Finalize_Handle (Object: Log_Message_Type; Handle: Log_Message_Handle_Type) is
   begin
      RDF.Raptor.Memory.Raptor_Free_Memory(Get_Handle(Object).Text);
      Finalize_Locator(Get_Handle(Object).Locator);
   end;

   procedure Adjust (Object: in out Log_Message_Type) is
   begin
      Get_Handle(Object).Text := RDF.Raptor.Memory.Copy_C_String(Get_Handle(Object).Text);
      -- FIXME: Locator
   end;

end RDF.Raptor.Log;
