with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body RDF.Raptor.WWW is

   procedure C_Raptor_Www_Set_User_Agent (WWW: WWW_Handle_Type; User_Agent: chars_ptr)
      with Import, Convention=>C, External_Name=>"raptor_www_set_user_agent";

   procedure Set_User_Agent (WWW: WWW_Type_Without_Finalize; User_Agent: String) is
   begin
      if User_Agent /= "" then
         declare
            Str: aliased char_array := To_C(User_Agent);
         begin
            C_Raptor_Www_Set_User_Agent(Get_Handle(WWW), To_Chars_Ptr(Str'Unchecked_Access));
         end;
      else
         C_Raptor_Www_Set_User_Agent(Get_Handle(WWW), Null_Ptr);
      end if;
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
      if Value /= "" then
         declare
            Str: aliased char_array := To_C(Value);
         begin
            C_Raptor_Www_Set_Http_Accept(Get_Handle(WWW), To_Chars_Ptr(Str'Unchecked_Access));
         end;
      else
         C_Raptor_Www_Set_Http_Accept(Get_Handle(WWW), Null_Ptr);
      end if;
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

   type C_Raptor_Www_Write_Bytes_Handler is access procedure (WWW: WWW_Handle_Type; User_data: chars_ptr; Ptr: chars_ptr; Size, Nmemb: size_t)
     with Convention=>C;

   type C_Raptor_Www_Content_Type_Handler is access procedure (WWW: WWW_Handle_Type; User_data: chars_ptr; Content_Type: chars_ptr)
     with Convention=>C;

end RDF.Raptor.WWW;
