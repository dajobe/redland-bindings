with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.C_Pointers;
with RDF.Raptor.Memory;
with RDF.Auxiliary.Convert; use RDF.Auxiliary.Convert;
with RDF.Raptor.URI;

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

   procedure raptor_www_set_user_agent (WWW: WWW_Handle_Type; User_Agent: chars_ptr)
      with Import, Convention=>C;

   procedure Set_User_Agent (WWW: WWW_Type_Without_Finalize; User_Agent: String) is
   begin
      Set_Or_Null(raptor_www_set_user_agent'Access, WWW, User_Agent);
   end;

   procedure raptor_www_set_proxy (WWW: WWW_Handle_Type; Proxy: char_array)
      with Import, Convention=>C;

   procedure Set_Proxy (WWW: WWW_Type_Without_Finalize; Proxy: String) is
   begin
      raptor_www_set_proxy(Get_Handle(WWW), To_C(Proxy));
   end;

   procedure raptor_www_set_http_accept (WWW: WWW_Handle_Type; Value: chars_ptr)
      with Import, Convention=>C;

   procedure Set_HTTP_Accept (WWW: WWW_Type_Without_Finalize; Value: String) is
   begin
      Set_Or_Null(raptor_www_set_http_accept'Access, WWW, Value);
   end;

   procedure raptor_set_cache_control (WWW: WWW_Handle_Type; Cache_Control: chars_ptr)
      with Import, Convention=>C;

   procedure Set_Cache_Control (WWW: WWW_Type_Without_Finalize; Cache_Control: String) is
      Str: aliased char_array := To_C(Cache_Control);
   begin
      raptor_set_cache_control(Get_Handle(WWW), To_Chars_Ptr(Str'Unchecked_Access));
   end;

   -- Remove Cache-Control: header altogether
   procedure Unset_Cache_Control (WWW: WWW_Type_Without_Finalize) is
   begin
      raptor_set_cache_control(Get_Handle(WWW), Null_Ptr);
   end;

   procedure raptor_www_set_connection_timeout (WWW: WWW_Handle_Type; Timeout: int)
      with Import, Convention=>C;

   procedure Set_Connection_Timeout (WWW: WWW_Type_Without_Finalize; Timeout: Natural) is
   begin
      raptor_www_set_connection_timeout(Get_Handle(WWW), int(Timeout));
   end;

   function raptor_www_get_final_uri (WWW: WWW_Handle_Type) return RDF.Raptor.URI.Handle_Type
      with Import, Convention=>C;

   function Get_Final_URI (WWW: WWW_Type_Without_Finalize) return RDF.Raptor.URI.URI_Type is
      use RDF.Raptor.URI;
   begin
      -- may return object with NULL handle
      return From_Handle(raptor_www_get_final_uri(Get_Handle(WWW)));
   end;

   function raptor_www_fetch (WWW: WWW_Handle_Type; URI: RDF.Raptor.URI.Handle_Type) return int
      with Import, Convention=>C;

   procedure Fetch (WWW: WWW_Type_Without_Finalize; URI: RDF.Raptor.URI.URI_Type_Without_Finalize) is
      use RDF.Raptor.URI;
   begin
      if raptor_www_fetch(Get_Handle(WWW), Get_Handle(URI)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   type String_P_Type is access all chars_ptr with Convention=>C;
   type String_P_Type2 is access all RDF.Auxiliary.C_Pointers.Pointer with Convention=>C;

   function Convert2 is new Ada.Unchecked_Conversion(String_P_Type, String_P_Type2);

   function raptor_www_fetch_to_string (WWW: WWW_Handle_Type;
                                        URI: RDF.Raptor.URI.Handle_Type;
                                        String_P: access chars_ptr;
                                        Length_P: access size_t;
                                        Malloc_Handler: chars_ptr) return int
       with Import, Convention=>C;

   function Fetch_To_String(WWW: WWW_Type_Without_Finalize; URI: RDF.Raptor.URI.URI_Type_Without_Finalize) return String is
      use RDF.Raptor.URI;
      String_P: aliased chars_ptr;
      Length_P: aliased size_t;
   begin
      if raptor_www_fetch_to_string(Get_Handle(WWW), Get_Handle(URI), String_P'Access, Length_P'Access, Null_Ptr) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      declare
         Result: constant String := Value_With_Possible_NULs(Convert2(String_P'Unchecked_Access).all, Length_P);
      begin
         RDF.Raptor.Memory.raptor_free_memory(String_P);
         return Result;
      end;
   end;

   function raptor_www_get_connection (WWW: WWW_Handle_Type) return Connection_Type
      with Import, Convention=>C;

   function Get_Connection (WWW: WWW_Type_Without_Finalize) return Connection_Type is
   begin
      return raptor_www_get_connection(Get_Handle(WWW));
   end;

   function raptor_www_set_ssl_cert_options (WWW: WWW_Handle_Type; Cert_Filename, Cert_Type, Cert_Passphrase: char_array) return int
      with Import, Convention=>C;

   procedure Set_SSL_Cert_Options (WWW: WWW_Type_Without_Finalize; Cert_Filename, Cert_Type, Cert_Passphrase: String) is
   begin
      if raptor_www_set_ssl_cert_options(Get_Handle(WWW), To_C(Cert_Filename), To_C(Cert_Type), To_C(Cert_Passphrase)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   function raptor_www_set_ssl_verify_options (WWW: WWW_Handle_Type; Verify_Peer, Verify_Host: int) return int
      with Import, Convention=>C;

   procedure Set_SSL_Verify_Options (WWW: WWW_Type_Without_Finalize; Verify_Peer, Verify_Host: Boolean) is
   begin
      if raptor_www_set_ssl_verify_options(Get_Handle(WWW),
                                           (if Verify_Peer then 1 else 0),
                                           (if Verify_Host then 1 else 0)) /= 0
      then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   procedure raptor_www_abort (WWW: WWW_Handle_Type; Reason: char_array)
      with Import, Convention=>C;

   procedure Abort_Operation (WWW: WWW_Type_Without_Finalize; Reason: String) is
   begin
      raptor_www_abort(Get_Handle(WWW), To_C(Reason));
   end;

   function raptor_new_www (World: RDF.Raptor.World.Raptor_World_Handle_Type) return WWW_Handle_Type
      with Import, Convention=>C;

   function New_WWW (World: RDF.Raptor.World.Raptor_World_Type'Class) return WWW_Type is
      use RDF.Raptor.World;
   begin
      return From_Non_Null_Handle(raptor_new_www(Get_Handle(World)));
   end;

   function raptor_new_www_with_connection (World: RDF.Raptor.World.Raptor_World_Handle_Type; Connection: Connection_Type) return WWW_Handle_Type
      with Import, Convention=>C;

   function New_WWW (World: RDF.Raptor.World.Raptor_World_Type'Class; Connection: Connection_Type) return WWW_Type is
      use RDF.Raptor.World;
   begin
      return From_Non_Null_Handle(raptor_new_www_with_connection(Get_Handle(World), Connection));
   end;

   procedure raptor_free_www (Handle: WWW_Handle_Type)
      with Import, Convention=>C;

   procedure Finalize_Handle (Object: WWW_Type; Handle: WWW_Handle_Type) is
   begin
      raptor_free_www(Handle);
   end;

   type User_Defined_Access is access constant WWW_Type_Without_Finalize'Class;
   function Ptr_To_Obj is new Ada.Unchecked_Conversion(chars_ptr, User_Defined_Access);
   function Obj_To_Ptr is new Ada.Unchecked_Conversion(User_Defined_Access, chars_ptr);

   type raptor_www_write_bytes_handler is access procedure (WWW: WWW_Handle_Type;
                                                              User_data: chars_ptr;
                                                              Ptr: RDF.Auxiliary.C_Pointers.Pointer;
                                                              Size, Nmemb: size_t)
     with Convention=>C;

   type raptor_www_content_type_handler is access procedure (WWW: WWW_Handle_Type; User_data: chars_ptr; Content_Type: chars_ptr)
      with Convention=>C;

   type raptor_www_uri_filter_func is access function (User_data: chars_ptr; URI: RDF.Raptor.URI.Handle_Type) return int
      with Convention=>C;

   type raptor_www_final_uri_handler is access procedure (WWW: WWW_Handle_Type; User_data: chars_ptr; URI: RDF.Raptor.URI.Handle_Type)
      with Convention=>C;

   procedure Write_Bytes_Handler_Impl (WWW: WWW_Handle_Type; User_data: chars_ptr; Ptr: RDF.Auxiliary.C_Pointers.Pointer; Size, Nmemb: size_t)
      with Convention=>C;

   procedure Content_Type_Handler_Impl (WWW: WWW_Handle_Type; User_data: chars_ptr; Content_Type: chars_ptr)
      with Convention=>C;

   function URI_Filter_Impl (User_data: chars_ptr; URI: RDF.Raptor.URI.Handle_Type) return int
      with Convention=>C;

   procedure Final_URI_Handler_Impl (WWW: WWW_Handle_Type; User_data: chars_ptr; URI: RDF.Raptor.URI.Handle_Type)
      with Convention=>C;

   procedure Write_Bytes_Handler_Impl (WWW: WWW_Handle_Type; User_data: chars_ptr; Ptr: RDF.Auxiliary.C_Pointers.Pointer; Size, Nmemb: size_t) is
   begin
      Write_Bytes_Handler(--WWW_Type_Without_Finalize'(From_Handle(WWW)), -- ignored
                          Ptr_To_Obj(User_Data).all,
                          Value_With_Possible_NULs(Ptr, Size*Nmemb));
   end;

   procedure Content_Type_Handler_Impl (WWW: WWW_Handle_Type; User_data: chars_ptr; Content_Type: chars_ptr) is
   begin
      Content_Type_Handler(--WWW_Type_Without_Finalize'(From_Handle(WWW)), -- ignored
                           Ptr_To_Obj(User_Data).all,
                           Value(Content_Type));
   end;

   function URI_Filter_Impl (User_data: chars_ptr; URI: RDF.Raptor.URI.Handle_Type) return int is
      use RDF.Raptor.URI;
      Result: constant Boolean := URI_Filter(Ptr_To_Obj(User_Data).all,
                                             RDF.Raptor.URI.URI_Type_Without_Finalize'(From_Non_Null_Handle(URI)));
   begin
      return (if Result then 0 else 1);
   end;

   procedure Final_URI_Handler_Impl (WWW: WWW_Handle_Type; User_Data: chars_ptr; URI: RDF.Raptor.URI.Handle_Type) is
      use RDF.Raptor.URI;
   begin
      Final_URI_Handler(--WWW_Type_Without_Finalize'(From_Handle(WWW)), -- ignored
                        Ptr_To_Obj(User_Data).all,
                        From_Handle(URI));
   end;

   procedure Initialize_All_Callbacks (WWW: WWW_Type_Without_Finalize) is
   begin
      Initialize_Write_Bytes_Handler(WWW);
      Initialize_Content_Type_Handler(WWW);
      Initialize_URI_Filter(WWW);
      Initialize_Final_URI_Handler(WWW);
   end;

   procedure raptor_www_set_write_bytes_handler(WWW: WWW_Handle_Type; Handler: raptor_www_write_bytes_handler; User_Data: chars_ptr)
      with Import, Convention=>C;

   procedure Initialize_Write_Bytes_Handler (WWW: WWW_Type_Without_Finalize) is
   begin
      raptor_www_set_write_bytes_handler(Get_Handle(WWW), Write_Bytes_Handler_Impl'Access, Obj_To_Ptr(WWW'Unchecked_Access));
   end;

   procedure raptor_www_set_content_type_handler (WWW: WWW_Handle_Type; Handler: raptor_www_content_type_handler; User_Data: chars_ptr)
      with Import, Convention=>C;

   procedure raptor_www_set_final_uri_handler (WWW: WWW_Handle_Type; Handler: raptor_www_final_uri_handler; User_Data: chars_ptr)
      with Import, Convention=>C;

   procedure Initialize_Content_Type_Handler (WWW: WWW_Type_Without_Finalize) is
   begin
      raptor_www_set_content_type_handler(Get_Handle(WWW), Content_Type_Handler_Impl'Access, Obj_To_Ptr(WWW'Unchecked_Access));
   end;

   procedure raptor_www_set_uri_filter (WWW: WWW_Handle_Type; Handler: raptor_www_uri_filter_func; User_Data: chars_ptr)
      with Import, Convention=>C;

   procedure Initialize_URI_Filter (WWW: WWW_Type_Without_Finalize) is
   begin
      raptor_www_set_uri_filter(Get_Handle(WWW), URI_Filter_Impl'Access, Obj_To_Ptr(WWW'Unchecked_Access));
   end;

   procedure Initialize_Final_URI_Handler (WWW: WWW_Type_Without_Finalize) is
   begin
      raptor_www_set_final_uri_handler(Get_Handle(WWW), Final_URI_Handler_Impl'Access, Obj_To_Ptr(WWW'Unchecked_Access));
   end;

end RDF.Raptor.WWW;
