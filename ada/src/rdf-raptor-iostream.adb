with Ada.Unchecked_Conversion;
with Ada.Text_IO; -- TODO: remove
with RDF.Raptor.World; use RDF.Raptor.World;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body RDF.Raptor.IOStream is

   function C_Raptor_New_Iostream_From_Sink (World: RDF.Raptor.World.Handle_Type) return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_iostream_from_sink";

   function From_Sink (World: RDF.Raptor.World.World_Type_Without_Finalize'Class) return Stream_Type_Without_Finalize is
   begin
      return From_Non_Null_Handle( C_Raptor_New_Iostream_From_Sink (Get_Handle (World)) );
   end;

   function C_Raptor_New_Iostream_From_Filename (World: RDF.Raptor.World.Handle_Type; filename: char_array) return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_iostream_from_filename";

   function From_Filename (World: RDF.Raptor.World.World_Type_Without_Finalize'Class; Filename: String) return Stream_Type_Without_Finalize is
   begin
      return From_Non_Null_Handle( C_Raptor_New_Iostream_From_Filename (Get_Handle (World), To_C (Filename)) );
   end;

   function C_Raptor_New_Iostream_From_File_Handle (World: RDF.Raptor.World.Handle_Type; File: RDF.Auxilary.C_File_Access) return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_iostream_from_file_handle";

   function From_File_Handle (World: RDF.Raptor.World.World_Type_Without_Finalize'Class; File: RDF.Auxilary.C_File_Access)
                              return Stream_Type_Without_Finalize is
   begin
      return From_Non_Null_Handle( C_Raptor_New_Iostream_From_File_Handle (Get_Handle (World), File) );
   end;

   function C_Raptor_New_Iostream_To_Sink (World: RDF.Raptor.World.Handle_Type) return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_iostream_to_sink";

   function To_Sink (World: RDF.Raptor.World.World_Type_Without_Finalize'Class) return Stream_Type_Without_Finalize is
   begin
      return From_Non_Null_Handle( C_Raptor_New_Iostream_To_Sink (Get_Handle (World)) );
   end;

   function C_Raptor_New_Iostream_To_Filename (World: RDF.Raptor.World.Handle_Type; filename: char_array) return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_iostream_to_filename";

   function To_Filename (World: RDF.Raptor.World.World_Type_Without_Finalize'Class; Filename: String) return Stream_Type_Without_Finalize is
   begin
      return From_Non_Null_Handle( C_Raptor_New_Iostream_To_Filename (Get_Handle (World), To_C (Filename)) );
   end;

--     function C_Raptor_New_Iostream_To_String (World: RDF.Raptor.World.Handle_Type; str: char_array; length: size_t) return Handle_Type
--       with Import, Convention=>C, External_Name=>"raptor_new_iostream_to_string";
--
--     function To_String (World: RDF.Raptor.World.World_Type_Without_Finalize'Class; Str: String)
--                           return Stream_Type_Without_Finalize is
--        Handle: constant Handle_Type := C_Raptor_New_Iostream_To_String (Get_Handle (World), To_C (Str), Str'Length);
--     begin
--        return From_Handle (Handle);
--     end;

   function C_Raptor_New_Iostream_To_File_Handle (World: RDF.Raptor.World.Handle_Type; File: RDF.Auxilary.C_File_Access) return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_iostream_to_file_handle";

   function To_File_Handle (World: RDF.Raptor.World.World_Type_Without_Finalize'Class; File: RDF.Auxilary.C_File_Access)
                              return Stream_Type_Without_Finalize is
   begin
      return From_Non_Null_Handle( C_Raptor_New_Iostream_To_File_Handle (Get_Handle (World), File) );
   end;

   function C_Raptor_Iostream_Hexadecimal_Write (value: unsigned; width: int; Stream: Handle_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_iostream_hexadecimal_write";

   procedure Hexadecimal_Write (Value: Natural; Width: Natural; Stream: Base_Stream_Type) is
   begin
      if C_Raptor_Iostream_Hexadecimal_Write(unsigned (Value), int(Width), Get_Handle (Stream)) < 0 then
         raise IOStream_Exception;
      end if;
   end;

   function C_Raptor_Iostream_Read_Bytes (Ptr: chars_ptr; size, nmemb: size_t; Stream: Handle_Type) return size_t
     with Import, Convention=>C, External_Name=>"raptor_iostream_read_bytes";

   function Read_Bytes (Ptr: chars_ptr; size, nmemb: size_t; Stream: Base_Stream_Type) return size_t is
      Result: constant size_t := C_Raptor_Iostream_Read_Bytes (Ptr, size, nmemb, Get_Handle (Stream));
   begin
      if Result < 0 then
         raise IOStream_Exception;
      end if;
      return Result;
   end;

   function C_Raptor_Iostream_Read_Eof (Stream: Handle_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_iostream_read_eof";

   function Read_Eof (Stream: Base_Stream_Type) return Boolean is
   begin
      return C_Raptor_Iostream_Read_Eof (Get_Handle (Stream)) /= 0;
   end;

   function C_Raptor_Iostream_Tell (Stream: Handle_Type) return unsigned_long
     with Import, Convention=>C, External_Name=>"raptor_iostream_tell";

   function Tell (Stream: Base_Stream_Type) return unsigned_long is
   begin
      return C_Raptor_Iostream_Tell (Get_Handle (Stream));
   end;

   function C_Raptor_Iostream_Counted_String_Write (str: char_array; len: size_t; stream: Handle_Type) return unsigned_long
     with Import, Convention=>C, External_Name=>"raptor_iostream_counted_string_write";

   procedure Write (Value: String; Stream: Base_Stream_Type) is
   begin
      if C_Raptor_Iostream_Counted_String_Write (To_C (Value, Append_Nul=>False), size_t (Value'Length), Get_Handle (Stream)) /= 0 then
         raise IOStream_Exception;
      end if;
   end;

   function C_Raptor_Iostream_Decimal_Write (Value: int; stream: Handle_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_iostream_decimal_write";

   procedure Decimal_Write (Value: int; Stream: Base_Stream_Type) is
   begin
      if C_Raptor_Iostream_Decimal_Write (Value, Get_Handle (Stream)) < 0 then
         raise IOStream_Exception;
      end if;
   end;

   function C_Raptor_Iostream_Write_Byte (byte: int; stream: Handle_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_iostream_write_byte";

   procedure Write (Value: char; Stream: Base_Stream_Type) is
   begin
      if C_Raptor_Iostream_Write_Byte (char'Pos(Value), Get_Handle (Stream)) /= 0 then
         raise IOStream_Exception;
      end if;
   end;

   function C_Raptor_Iostream_Write_Bytes (Ptr: chars_ptr; size, nmemb: size_t; Stream: Handle_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_iostream_write_bytes";

   function Write_Bytes (Ptr: chars_ptr; size, nmemb: size_t; Stream: Base_Stream_Type) return int is
      Result: constant int := C_Raptor_Iostream_Write_Bytes (Ptr, size, nmemb, Get_Handle (Stream));
   begin
      if Result < 0 then
         raise IOStream_Exception;
      end if;
      return Result;
   end;

   function C_Raptor_Iostream_Write_End (Stream: Handle_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_iostream_write_end";

   procedure Write_End (Stream: Base_Stream_Type) is
   begin
      if C_Raptor_Iostream_Write_End (Get_Handle (Stream)) /= 0 then
         raise IOStream_Exception;
      end if;
   end;

   function C_Raptor_Bnodeid_Ntriples_Write (bnode: char_array; len: size_t; Stream: Handle_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_bnodeid_ntriples_write";

   procedure Bnodeid_Ntriples_Write (bnode: String; Stream: Base_Stream_Type) is
   begin
      if C_Raptor_Bnodeid_Ntriples_Write (To_C (bnode, Append_Nul=>False), bnode'Length, Get_Handle (Stream)) /= 0 then
         raise IOStream_Exception;
      end if;
   end;

   function C_raptor_string_escaped_write (Str: char_array; Len: size_t; Delim: char; Flags: unsigned; Stream: Handle_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_string_escaped_write";

   procedure Escaped_Write (Value: String; Delim: Character; Flags: Escaped_Write_Bitflags.Bitflags; Stream: Base_Stream_Type) is
   begin
      if C_raptor_string_escaped_write (To_C (Value, Append_Nul=>False), Value'Length, To_C (Delim), unsigned (Flags), Get_Handle (Stream)) /= 0 then
         raise IOStream_Exception;
      end if;
   end;

   function C_Raptor_String_Ntriples_Write (Str: char_array; Len: size_t; Delim: char; Stream: Handle_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_string_ntriples_write";

   procedure Ntriples_Write (Value: String; Delim: Character; Stream: Base_Stream_Type) is
   begin
      if C_Raptor_String_Ntriples_Write (To_C (Value, Append_Nul=>False), Value'Length, To_C (Delim), Get_Handle (Stream)) /= 0 then
         raise IOStream_Exception;
      end if;
   end;

   function C_Raptor_String_Python_Write (ptr: char_array; len: size_t; delim: char; mode: Python_Write_Mode; Stream: Handle_Type) return int
     with Import, Convention=>C, External_Name=>"raptor_string_python_write";

   procedure String_Python_Write (Value: String; Delim: Character; Mode: Python_Write_Mode; Stream: Base_Stream_Type) is
   begin
      if C_Raptor_String_Python_Write (To_C (Value, Append_Nul=>False), Value'Length, To_C (Delim), Mode, Get_Handle (Stream)) /= 0 then
         raise IOStream_Exception;
      end if;
   end;

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
      Result: constant int := Do_Write_Bytes (Ptr_To_Obj (context).all, ptr, size, nmemb);
   begin
      return Result;
   exception
      when others =>
         return -1;
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

   function Open (World: RDF.Raptor.World.World_Type_Without_Finalize'Class) return User_Defined_Stream_Type is
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

   procedure Do_Write_Byte (Stream: in out User_Defined_Stream_Type; Byte: char) is
      Byte2: aliased char_array := (1=>Byte);
   begin
      if Do_Write_Bytes (Stream, To_Chars_Ptr (Byte2'Unchecked_Access), 1, 1) /= 1 then
         raise IOStream_exception;
      end if;
   end;

   function Do_Write_Bytes (Stream: in out User_Defined_Stream_Type; Data: chars_ptr; Size, Count: size_t) return int is
   begin
      raise Program_Error;
      return 0;
   end;

   procedure Do_Write_End (Stream: in out User_Defined_Stream_Type) is
   begin
      raise Program_Error;
   end;

   function Do_Read_Bytes (Stream: in out User_Defined_Stream_Type; Data: chars_ptr; Size, Count: size_t) return size_t is
   begin
      raise Program_Error;
      return 0;
   end;

   function Do_Read_Eof (Stream: in out User_Defined_Stream_Type) return Boolean is
   begin
      raise Program_Error;
      return False;
   end;

   function From_Handle(Handle: Handle_Type) return Stream_From_String is
   begin
      raise Program_Error;
      return (Base_Stream_Type'(From_Handle (Handle)) with Length=>0, Str=>"");
   end;

   function From_Non_Null_Handle(Handle: Handle_Type) return Stream_From_String is
   begin
      raise Program_Error;
      return (Base_Stream_Type'(From_Handle (Handle)) with Length=>0, Str=>"");
   end;

   function C_Raptor_New_Iostream_From_String (World: RDF.Raptor.World.Handle_Type; str: char_array; length: size_t) return Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_iostream_from_string";

   function Open_From_String (World: RDF.Raptor.World.World_Type'Class; Value: String) return Stream_From_String is
   begin
      return Stream: Stream_From_String(Value'Length) do
         Stream.Str := To_C (Value, Append_Nul=>False);
         Set_Handle_Hack (Stream, C_Raptor_New_Iostream_From_String (Get_Handle (World), Stream.Str, Value'Length));
      end return;
   end;

   function From_Handle(Handle: Handle_Type) return Stream_To_String is
   begin
      raise Program_Error;
      return (Base_Stream_Type'(From_Handle (Handle)) with Str=>Ada.Strings.Unbounded.Null_Unbounded_String);
   end;

   function From_Non_Null_Handle(Handle: Handle_Type) return Stream_To_String is
   begin
      raise Program_Error;
      return (Base_Stream_Type'(From_Handle (Handle)) with Str=>Ada.Strings.Unbounded.Null_Unbounded_String);
   end;

   function Open (World: RDF.Raptor.World.World_Type_Without_Finalize'Class) return Stream_To_String is
   begin
      return (User_Defined_Stream_Type'(Open (World)) with Str=>Ada.Strings.Unbounded.Null_Unbounded_String);
   end;

   function Value (Stream: Stream_To_String) return String is
   begin
      return To_String (Stream.Str);
   end;

   function Do_Write_Bytes (Stream: in out Stream_To_String; Data: chars_ptr; Size, Count: size_t) return int is
   begin
      Append(Stream.Str, Value (Data, Size*Count));
      return int(Size*Count);
   end;

end RDF.Raptor.IOStream;
