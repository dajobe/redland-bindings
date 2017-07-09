with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.C_Pointers;
with RDF.Auxiliary.Convert; use RDF.Auxiliary.Convert;

package body RDF.Raptor.WWW is

   type C_Func is access procedure (WWW: WWW_Handle_Type; Value: chars_ptr)
      with Convention=>C;

   procedure Set_Or_Null (Func: C_Func; WWW: WWW_Type_Without_Finalize; Value: String) is
   begin
      if Value /= "" then
         declare
            Str: aliased char_array := To_C(Value);
         begin
            Func.all(Get_Handle(WWW), To_Chars_Ptr(Str'Unchecked_Access));
         end;
      else
         Func.all(Get_Handle(WWW), Null_Ptr);
      end if;
   end;

   procedure C_Raptor_Www_Set_User_Agent (WWW: WWW_Handle_Type; User_Agent: chars_ptr)
      with Import, Convention=>C, External_Name=>"raptor_www_set_user_agent";

   procedure Set_User_Agent (WWW: WWW_Type_Without_Finalize; User_Agent: String) is
   begin
      Set_Or_Null(C_Raptor_Www_Set_User_Agent'Access, WWW, User_Agent);
   end;

   procedure C_Raptor_Www_Set_Proxy (WWW: WWW_Handle_Type; Proxy: char_array)
      with Import, Convention=>C, External_Name=>"raptor_www_set_proxy";

   procedure Set_Proxy (WWW: WWW_Type_Without_Finalize; Proxy: String) is
   begin
      C_Raptor_Www_Set_Proxy(Get_Handle(WWW), To_C(Proxy));
   end;

   procedure C_Raptor_Www_Set_Http_Accept (WWW: WWW_Handle_Type; Value: chars_ptr)
      with Import, Convention=>C, External_Name=>"raptor_www_set_http_accept";

   procedure Set_HTTP_Accept (WWW: WWW_Type_Without_Finalize; Value: String) is
   begin
      Set_Or_Null(C_Raptor_Www_Set_Http_Accept'Access, WWW, Value);
   end;

   procedure C_Raptor_Set_Cache_Control (WWW: WWW_Handle_Type; Cache_Control: chars_ptr)
      with Import, Convention=>C, External_Name=>"raptor_www_set_cache_control";

   procedure Set_Cache_Control (WWW: WWW_Type_Without_Finalize; Cache_Control: String) is
      Str: aliased char_array := To_C(Cache_Control);
   begin
      C_Raptor_Set_Cache_Control(Get_Handle(WWW), To_Chars_Ptr(Str'Unchecked_Access));
   end;

   -- Remove Cache-Control: header altogether
   procedure Unset_Cache_Control (WWW: WWW_Type_Without_Finalize) is
   begin
      C_Raptor_Set_Cache_Control(Get_Handle(WWW), Null_Ptr);
   end;

   function C_Raptor_New_WWW (World: RDF.Raptor.World.Handle_Type) return WWW_Handle_Type
      with Import, Convention=>C, External_Name=>"raptor_new_www";

   function New_WWW (World: RDF.Raptor.World.World_Type'Class) return WWW_Type is
      use RDF.Raptor.World;
   begin
      return From_Non_Null_Handle(C_Raptor_New_WWW(Get_Handle(World)));
   end;

   function C_Raptor_New_WWW_With_Connection (World: RDF.Raptor.World.Handle_Type; Connection: Connection_Type) return WWW_Handle_Type
      with Import, Convention=>C, External_Name=>"raptor_new_www_with_connection";

   function New_WWW (World: RDF.Raptor.World.World_Type'Class; Connection: Connection_Type) return WWW_Type is
      use RDF.Raptor.World;
   begin
      return From_Non_Null_Handle(C_Raptor_New_WWW_With_Connection(Get_Handle(World), Connection));
   end;

   procedure C_Raptor_Free_WWW (Handle: WWW_Handle_Type)
      with Import, Convention=>C, External_Name=>"raptor_free_www";

   procedure Finalize_Handle (Object: WWW_Type; Handle: WWW_Handle_Type) is
   begin
      C_Raptor_Free_WWW(Handle);
   end;

   type User_Defined_Access is access constant WWW_Type_Without_Finalize'Class;
   function Ptr_To_Obj is new Ada.Unchecked_Conversion(chars_ptr, User_Defined_Access);
   function Obj_To_Ptr is new Ada.Unchecked_Conversion(User_Defined_Access, chars_ptr);

   type C_Raptor_Www_Write_Bytes_Handler is access procedure (WWW: WWW_Handle_Type;
                                                              User_data: chars_ptr;
                                                              Ptr: RDF.Auxiliary.C_Pointers.Pointer;
                                                              Size, Nmemb: size_t)
     with Convention=>C;

   type C_Raptor_Www_Content_Type_Handler is access procedure (WWW: WWW_Handle_Type; User_data: chars_ptr; Content_Type: chars_ptr)
      with Convention=>C;

   procedure Write_Bytes_Handler_Impl (WWW: WWW_Handle_Type; User_data: chars_ptr; Ptr: RDF.Auxiliary.C_Pointers.Pointer; Size, Nmemb: size_t)
      with Convention=>C;

   procedure Write_Bytes_Handler_Impl (WWW: WWW_Handle_Type; User_data: chars_ptr; Ptr: RDF.Auxiliary.C_Pointers.Pointer; Size, Nmemb: size_t) is
   begin
      Write_Bytes_Handler(--WWW_Type_Without_Finalize'(From_Handle(WWW)), -- ignored
                          Ptr_To_Obj(User_Data).all,
                          Value_With_Possible_NULs(Ptr, Size*Nmemb));
   end;

   procedure Initialize_All_Callbacks (WWW: WWW_Type_Without_Finalize) is
   begin
      Initialize_Write_Bytes_Handler(WWW);
      -- TODO
   end;

   procedure C_raptor_www_set_write_bytes_handler (WWW: WWW_Handle_Type; Handler: C_Raptor_Www_Write_Bytes_Handler; User_Data: chars_ptr)
      with Import, Convention=>C, External_Name=>"raptor_www_set_write_bytes_handler";

   procedure Initialize_Write_Bytes_Handler (WWW: WWW_Type_Without_Finalize) is
   begin
      C_raptor_www_set_write_bytes_handler(Get_Handle(WWW), Write_Bytes_Handler_Impl'Access, Obj_To_Ptr(WWW'Unchecked_Access));
   end;

end RDF.Raptor.WWW;
