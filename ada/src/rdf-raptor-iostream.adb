with Ada.Unchecked_Conversion;

package body RDF.Raptor.IOStream is

   procedure C_Raptor_Free_Iostream(Handle: Handle_Type)
     with Import, Convention=>C, External_Name=>"raptor_free_iostream";

   procedure Finalize_Handle(Object: Stream_Type; Handle: Handle_Type) is
   begin
      C_Raptor_Free_Iostream (Handle);
   end;

   type raptor_iostream_init_func is access function (context: chars_ptr) return int
      with Convention=>C;
   type raptor_iostream_finish_func is access procedure (context: chars_ptr)
      with Convention=>C;
   type raptor_iostream_write_byte_func is access function (context: chars_ptr; byte: int) return int
      with Convention=>C;
   type raptor_iostream_write_bytes_func is access function (context: chars_ptr; ptr: chars_ptr; size, nmemb: size_t) return int
      with Convention=>C;
   type raptor_iostream_write_end_func is access function (context: chars_ptr) return int
      with Convention=>C;
   type raptor_iostream_read_bytes_func is access function (context: chars_ptr; ptr: chars_ptr; size, nmemb: size_t) return int
      with Convention=>C;
   type raptor_iostream_read_eof_func is access function (context: chars_ptr) return int
      with Convention=>C;

   type Dispatcher_Type is
      record
         version    : int := 2;
         -- V1 functions
         init       : raptor_iostream_init_func   := null;
         finish     : raptor_iostream_finish_func := null;
         write_byte : raptor_iostream_write_byte_func;
         write_bytes: raptor_iostream_write_bytes_func;
         write_end  : raptor_iostream_write_end_func;
         -- V2 functions
         read_bytes : raptor_iostream_read_bytes_func;
         read_eof   : raptor_iostream_read_eof_func;
      end record
   	 with Convention=>C;

   type User_Defined_Access is access all User_Defined_Stream_Type'Class;
   function Ptr_To_Obj is new Ada.Unchecked_Conversion(chars_ptr, User_Defined_Access);
   function Obj_To_Ptr is new Ada.Unchecked_Conversion(User_Defined_Access, chars_ptr);

--     function raptor_iostream_init_impl (context: chars_ptr) return int
--        with Convention=>C;
--     procedure raptor_iostream_finish_impl (context: chars_ptr)
--        with Convention=>C;
   function raptor_iostream_write_byte_impl (context: chars_ptr; byte: int) return int
      with Convention=>C;
   function raptor_iostream_write_bytes_impl (context: chars_ptr; ptr: chars_ptr; size, nmemb: size_t) return int
      with Convention=>C;
   function raptor_iostream_write_end_impl (context: chars_ptr) return int
      with Convention=>C;
   function raptor_iostream_read_bytes_impl (context: chars_ptr; ptr: chars_ptr; size, nmemb: size_t) return int
      with Convention=>C;
   function raptor_iostream_read_eof_impl (context: chars_ptr) return int
      with Convention=>C;

   function raptor_iostream_write_byte_impl (context: chars_ptr; byte: int) return int is
   begin
      Do_Write_Byte (Ptr_To_Obj (context).all, char'Val(byte));
      return 0;
   exception
      when others =>
         return 1;
   end;

   function raptor_iostream_write_bytes_impl (context: chars_ptr; ptr: chars_ptr; size, nmemb: size_t) return int is
   begin
      Do_Write_Bytes (Ptr_To_Obj (context).all, ptr, size, nmemb);
      return 0;
   exception
      when others =>
         return 1;
   end;

   function raptor_iostream_write_end_impl (context: chars_ptr) return int is
   begin
      Do_Write_End (Ptr_To_Obj (context).all);
      return 0;
   exception
      when others =>
         return 1;
   end;

   function raptor_iostream_read_bytes_impl (context: chars_ptr; ptr: chars_ptr; size, nmemb: size_t) return int is
      Ret: constant size_t := Do_Read_Bytes (Ptr_To_Obj (context).all, ptr, size, nmemb);
   begin
      return int(Ret);
   exception
      when others =>
         return -1;
   end;

   function raptor_iostream_read_eof_impl (context: chars_ptr) return int is
   begin
      return (if Do_Read_Eof (Ptr_To_Obj (context).all) then 1 else 0);
   end;

   Dispatch: aliased constant Dispatcher_Type :=
     (version => 2,
      init => null,
      finish => null,
      write_byte => raptor_iostream_write_byte_impl'Access,
      write_bytes=> raptor_iostream_write_bytes_impl'Access,
      write_end  => raptor_iostream_write_end_impl'Access,
      read_bytes => raptor_iostream_read_bytes_impl'Access,
      read_eof   => raptor_iostream_read_eof_impl'Access);

   function C_raptor_new_iostream_from_handler(world: RDF.Raptor.World.Handle_Type;
                                               user_data: chars_ptr;
                                               Dispatcher: access constant Dispatcher_Type)
                                               return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_iostream_from_handler";

   function Open (World: RDF.Raptor.World.World_Type) return User_Defined_Stream_Type is
      Handle: Handle_Type;
      use all type RDF.Raptor.World.World_Type;
   begin
      return Stream: User_Defined_Stream_Type do
         Handle := C_raptor_new_iostream_from_handler (Get_Handle (World),
                                                       Obj_To_Ptr (User_Defined_Stream_Type'Class(Stream)'Unchecked_Access),
                                                       Dispatch'Access);
         Set_Handle_Hack (Stream, Handle);
      end return;
   end;

   -- TODO: We can implement this with Write_Bytes procedure
   procedure Do_Write_Byte (Stream: User_Defined_Stream_Type; Byte: char) is
   begin
      raise Program_Error;
   end;

   procedure Do_Write_Bytes (Stream: User_Defined_Stream_Type; Data: chars_ptr; Size, Count: size_t) is
   begin
      raise Program_Error;
   end;

   procedure Do_Write_End (Stream: User_Defined_Stream_Type) is
   begin
      raise Program_Error;
   end;

   function Do_Read_Bytes (Stream: User_Defined_Stream_Type; Data: chars_ptr; Size, Count: size_t) return size_t is
   begin
      raise Program_Error;
      return 0;
   end;

   function Do_Read_Eof (Stream: User_Defined_Stream_Type) return Boolean is
   begin
      raise Program_Error;
      return False;
   end;

end RDF.Raptor.IOStream;
