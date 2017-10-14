with Ada.Unchecked_Conversion;
--  with System.Address_To_Access_Conversions;
with RDF.Auxiliary.Convert_Void;
with System; use System;

package body RDF.Raptor.Log is

   function Get_URI (Locator: Locator_Type) return URI_Type_Without_Finalize is
   begin
      return From_Handle(Get_Handle(Locator).URI);
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

--     type User_Defined_Access is access constant Log_Handler'Class;
   package My_Conv is new RDF.Auxiliary.Convert_Void(Log_Handler'Class);

   procedure Our_Raptor_Log_Handler(Data: chars_ptr; Msg: Log_Message_Type) is
   begin
      Log_Message(My_Conv.To_Access(Data).all, Msg);
   end;

   function raptor_world_set_log_handler(World: Raptor_World_Handle;
                                         Data: chars_ptr;
                                         Handler: Log_Handler_Procedure_Type)
                                         return int
     with Import, Convention=>C;

   procedure Set_Log_Handler(World: in out Raptor_World_Type_Without_Finalize'Class; Handler: access Log_Handler) is
   begin
      if raptor_world_set_log_handler(Get_Handle(World), My_Conv.To_C_Pointer(Handler), Our_Raptor_Log_Handler'Access) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_log_level_get_label(Level: Log_Level_Type) return chars_ptr
     with Import, Convention=>C;

   function Get_Label (Level: Log_Level_Type) return String is
   begin
      return Value(raptor_log_level_get_label(Level));
   end;

   function raptor_domain_get_label (Level: Domain_Type) return chars_ptr
     with Import, Convention=>C;

   function Get_Label (Level: Domain_Type) return String is
   begin
      return Value(raptor_domain_get_label(Level));
   end;

   function raptor_locator_print (Locator: Locator_Handle; Stream: RDF.Auxiliary.C_File_Access) return int
     with Import, Convention=>C;

   procedure Print (Locator: Locator_Type; File: RDF.Auxiliary.C_File_Access) is
   begin
      if raptor_locator_print(Get_Handle(Locator), File) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_locator_format (Buffer: chars_ptr; Length: size_t; Locator: Locator_Handle) return int
     with Import, Convention=>C;

   function Format (Locator: Locator_Type) return String is
      Res1: constant int := raptor_locator_format(Null_Ptr, 0, Get_Handle(Locator));
   begin
      if Res1 < 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      declare
         Buffer: aliased char_array := (1..size_t(Res1) => Interfaces.C.Nul);
      begin
         if raptor_locator_format(To_Chars_Ptr(Buffer'Unchecked_Access, Nul_Check=>False), 0, Get_Handle(Locator)) < 0
         then
            raise RDF.Auxiliary.RDF_Exception;
         end if;
         return To_Ada(Buffer);
      end;
   end;

   procedure raptor_free_uri (Handle: URI_Handle)
     with Import, Convention=>C;

   function raptor_uri_copy (Handle: URI_Handle)
                             return URI_Handle
     with Import, Convention=>C;

   procedure Finalize_Locator (Handle: Locator_Handle) is
      package My_Conv is new RDF.Auxiliary.Convert_Void(Locator_Type_Record);
   begin
      raptor_free_uri(Handle.URI);
      RDF.Raptor.Memory.Raptor_Free_Memory(Handle.File);
      RDF.Raptor.Memory.Raptor_Free_Memory(My_Conv.To_C_Pointer(My_Conv.Object_Pointer(Handle)));
   end;

   procedure Finalize_Handle (Object: Locator_Type; Handle: Locator_Handle) is
   begin
      Finalize_Locator(Handle);
   end;

   package Locator_Conv is new RDF.Auxiliary.Convert_Void(Locator_Type_Record);

   function Adjust_Handle (Object: Locator_Type; Handle: Locator_Handle) return Locator_Handle is
      Size: constant size_t := size_t((Locator_Type'Max_Size_In_Storage_Elements * Storage_Unit + (char'Size-1)) / char'Size);
      Result2: constant chars_ptr := RDF.Raptor.Memory.raptor_alloc_memory(Size);
      Result: constant Locator_Handle := Locator_Handle(Locator_Conv.To_Access(Result2));
   begin
      Result.all := Handle.all;
      Result.URI := raptor_uri_copy(Handle.URI);
      Result.File := RDF.Raptor.Memory.Copy_C_String(Handle.File);
      return Result;
   end;

   procedure Finalize_Handle (Object: Log_Message_Type; Handle: Log_Message_Handle) is
      package My_Conv is new RDF.Auxiliary.Convert_Void(Log_Message_Record);
   begin
      RDF.Raptor.Memory.Raptor_Free_Memory(Handle.Text);
      Finalize_Locator(Handle.Locator);
      RDF.Raptor.Memory.Raptor_Free_Memory(My_Conv.To_C_Pointer(My_Conv.Object_Pointer(Handle)));
   end;

   package Log_Message_Conv is new RDF.Auxiliary.Convert_Void(Log_Message_Record);

   function Adjust_Handle (Object: Log_Message_Type; Handle: Log_Message_Handle)
                           return Log_Message_Handle is
      Size: constant size_t := size_t((Log_Message_Type'Max_Size_In_Storage_Elements * Storage_Unit + (char'Size-1)) / char'Size);
      Result2: constant chars_ptr := RDF.Raptor.Memory.raptor_alloc_memory(Size);
      Result: constant Log_Message_Handle := Log_Message_Handle(Log_Message_Conv.To_Access(Result2));
   begin
      Result.Text := RDF.Raptor.Memory.Copy_C_String(Handle.Text);
      Result.Locator.URI := raptor_uri_copy(Handle.Locator.URI);
      Result.Locator.File := RDF.Raptor.Memory.Copy_C_String(Handle.Locator.File);
      return Result;
   end;

end RDF.Raptor.Log;
