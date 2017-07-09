with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Interfaces.C.Pointers;
with RDF.Raptor.World; use RDF.Raptor.World;

package body RDF.Raptor.Syntaxes is

   package String_Ptrs is new Interfaces.C.Pointers(size_t, chars_ptr, chars_ptr_array, Null_Ptr);

   use String_Ptrs;

   Mime_Type_Q_Default: constant Mime_Type_Q := Mime_Type_Q'(Mime_Type=>Null_Ptr, Mime_Type_Len=>0, Q=>0); -- hack
   type Mime_Type_Q_Array is array (size_t range <>) of aliased Mime_Type_Q;
   package Mime_Type_Q_Ptrs is new Interfaces.C.Pointers(size_t, Mime_Type_Q, Mime_Type_Q_Array, Mime_Type_Q_Default);

   use Mime_Type_Q_Ptrs;

   function Get_MIME_Type (Object: Mime_Type_Q) return String is
   begin
      return Value(Object.Mime_Type, Object.Mime_Type_Len);
   end;

   function Get_Q (Object: Mime_Type_Q) return Q_Type is (Q_Type(Object.Q));

   function Get_Name (Object: Syntax_Description_Type; Index: Natural) return String is
      Ptr: constant access chars_ptr := String_Ptrs.Pointer(Object.Names) + ptrdiff_t(Index);
   begin
      return Value(Ptr.all);
   end;

   function Get_Names_Count (Object: Syntax_Description_Type) return Natural is (Natural(Object.Names_Count));

   function Get_Label (Object: Syntax_Description_Type) return String is
   begin
      return Value(Object.Label);
   end;

   function Get_MIME_Type (Object: Syntax_Description_Type; Index: Natural) return Mime_Type_Q is
      Ptr: constant access Mime_Type_Q := Mime_Type_Q_Ptrs.Pointer(Object.Mime_Types) + ptrdiff_t(Index);
   begin
      return Ptr.all;
   end;

   function Get_MIME_Types_Count (Object: Syntax_Description_Type) return Natural is (Natural(Object.Mime_Types_Count));

   function Get_URI (Object: Syntax_Description_Type; Index: Natural) return URI_String is
      Ptr: constant access chars_ptr := String_Ptrs.Pointer(Object.URI_Strings) + ptrdiff_t(Index);
   begin
      return URI_String(String'(Value(Ptr.all)));
   end;

   function Get_URIs_Count (Object: Syntax_Description_Type) return Natural is (Natural(Object.URI_Strings_Count));

   function Get_Flags (Object: Syntax_Description_Type) return Syntax_Bitflags is
   begin
      return Object.Flags;
   end;

   function raptor_world_is_parser_name(World: RDF.Raptor.World.Handle_Type; Name: char_array) return int
     with Import, Convention=>C;

   function Is_Parser_Name (World: World_Type_Without_Finalize'Class; Name: String) return Boolean is
   begin
      return raptor_world_is_parser_name(Get_Handle(World), To_C(Name, Append_Nul=>True)) /= 0;
   end;

   function raptor_world_guess_parser_name (World: RDF.Raptor.World.Handle_Type;
                                              URI: RDF.Raptor.URI.Handle_Type;
                                              MIME_Type: Char_Array;
                                              Buffer: Char_Array;
                                              Len: Size_T;
                                              Identifier: Char_Array) return chars_ptr
     with Import, Convention=>C;


   function Guess_Parser_Name (World: World_Type_Without_Finalize'Class; URI: URI_Type; MIME_Type: String; Buffer: String; Identifier: String)
                               return String is
   begin
      return Value( raptor_world_guess_parser_name(Get_Handle(World),
                                                     Get_Handle(URI),
                                                     To_C(MIME_Type, Append_Nul=>True),
                                                     To_C(Buffer, Append_Nul=>False),
                                                     size_t(Buffer'Length),
                    To_C(Identifier, Append_Nul=>True)) );
   end;

   function raptor_world_is_serializer_name(World: RDF.Raptor.World.Handle_Type; Name: char_array) return int
     with Import, Convention=>C;

   function Is_Serializer_Name (World: World_Type_Without_Finalize'Class; Name: String) return Boolean is
   begin
      return raptor_world_is_serializer_name(Get_Handle(World), To_C(Name, Append_Nul=>True)) /= 0;
   end;

   function Get_Position (Cursor: Parser_Description_Cursor    ) return Natural is (Cursor.Position);
   function Get_Position (Cursor: Serializer_Description_Cursor) return Natural is (Cursor.Position);

   function raptor_world_get_parser_description (World: RDF.Raptor.World.Handle_Type; Counter: unsigned) return access Syntax_Description_Type
     with Import, Convention=>C;

   function raptor_world_get_serializer_description (World: RDF.Raptor.World.Handle_Type; Counter: unsigned) return access Syntax_Description_Type
     with Import, Convention=>C;

   function Get_Description (Cursor: Parser_Description_Cursor    ) return Syntax_Description_Type is
   begin
      return raptor_world_get_parser_description(Cursor.World, unsigned(Cursor.Position)).all;
   end;

   function Get_Description (Cursor: Serializer_Description_Cursor) return Syntax_Description_Type is
   begin
      return raptor_world_get_serializer_description(Cursor.World, unsigned(Cursor.Position)).all;
   end;

   function Has_Element (Position: Parser_Description_Cursor) return Boolean is
   begin
      return raptor_world_get_parser_description(Position.World, unsigned(Position.Position)) /= null;
   end;

   function Has_Element (Position: Serializer_Description_Cursor) return Boolean is
   begin
      return raptor_world_get_serializer_description(Position.World, unsigned(Position.Position)) /= null;
   end;

   function First (Object: Parser_Description_Iterator) return Parser_Description_Cursor is
   begin
      return (Position=>0, World=>Object.World);
   end;

   function Next (Object: Parser_Description_Iterator; Position: Parser_Description_Cursor) return Parser_Description_Cursor is
   begin
      return (Position=>Position.Position+1, World=>Position.World);
   end;

   function First (Object: Serializer_Description_Iterator) return Serializer_Description_Cursor is
   begin
      return (Position=>0, World=>Object.World);
   end;

   function Next (Object: Serializer_Description_Iterator; Position: Serializer_Description_Cursor) return Serializer_Description_Cursor is
   begin
      return (Position=>Position.Position+1, World=>Position.World);
   end;

   function Create_Parser_Descriptions_Iterator(World: RDF.Raptor.World.World_Type_Without_Finalize'Class) return Parser_Description_Iterator is
   begin
      return (World=>Get_Handle(World));
   end;

   function Create_Serializer_Descriptions_Iterator (World: RDF.Raptor.World.World_Type_Without_Finalize'Class) return Serializer_Description_Iterator is
   begin
      return (World=>Get_Handle(World));
   end;

   --     function Parser_Descriptions     (World: World_Type_Without_Finalize'Class) return Parser_Description_List is (World=>Get_Handle(World));
--
--     function Serializer_Descriptions (World: World_Type_Without_Finalize'Class) return Serializer_Description_List is (World=>Get_Handle(World));

end RDF.Raptor.Syntaxes;
