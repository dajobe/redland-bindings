with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxilary.C_String_Holders;
with RDF.Raptor.Memory;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Namespaces; use RDF.Raptor.Namespaces;
with RDF.Raptor.Namespaces_Stacks; use RDF.Raptor.Namespaces_Stacks;

package body RDF.Raptor.Term is

   function Get_World (Term: Term_Type_Without_Finalize) return RDF.Raptor.World.World_Type_Without_Finalize is
   begin
      return From_Handle( Get_Handle(Term).World );
   end;

   function Get_Kind (Term: Term_Type_Without_Finalize) return Term_Kind is
   begin
      return Get_Handle(Term).Kind;
   end;

   function Get_URI (Term: Term_Type_Without_Finalize) return URI_Type_Without_Finalize is
   begin
      return From_Handle( Get_Handle(Term).Value.URI );
   end;

   function Get_Literal (Term: Term_Type_Without_Finalize) return Term_Literal_Value is
   begin
      return Get_Handle(Term).Value.Literal;
   end;

   function Get_Blank (Term: Term_Type_Without_Finalize) return Term_Blank_Value is
   begin
      return Get_Handle(Term).Value.Blank;
   end;

   function Value (Literal: Term_Literal_Value) return String is
   begin
      return Value(Literal.Str, Size_T(Literal.Len));
   end;

   function Datatype (Literal: Term_Literal_Value) return RDF.Raptor.URI.URI_Type is
      use type RDF.Raptor.URI.Handle_Type;
   begin
      if Literal.Datatype = null then
         return From_Handle(null);
      end if;
      return From_Handle(Literal.Datatype);
   end;

   function Language (Literal: Term_Literal_Value) return String is
   begin
      if Literal.Language = Null_Ptr then
         return "";
      end if;
      return Value(Literal.Language, size_t(Literal.Language_Len));
   end;

   function Value (Blank: Term_Blank_Value) return String is
   begin
      return Value(Blank.Str, size_t(Blank.Len));
   end;

   function C_Raptor_New_Term_From_Counted_Blank (World: RDF.Raptor.World.Handle_Type;
                                                  Blank: chars_ptr;
                                                  Length: size_t)
                                                  return Term_Handle
     with Import, Convention=>C, External_Name=>"raptor_new_term_from_counted_blank";

   function From_Blank (World: World_Type_Without_Finalize'Class) return Term_Type is
      use RDF.Raptor.World;
   begin
      return From_Non_Null_Handle(C_Raptor_New_Term_From_Counted_Blank(Get_Handle(World), Null_Ptr, 0));
   end;

   function From_Blank (World: World_Type_Without_Finalize'Class; ID: String) return Term_Type is
      use RDF.Raptor.World;
      Str: aliased char_array := To_C(ID);
   begin
      return From_Non_Null_Handle(C_Raptor_New_Term_From_Counted_Blank(Get_Handle(World), To_Chars_Ptr(Str'Unchecked_Access), ID'Length));
   end;

   function From_Blank (World: World_Type_Without_Finalize'Class; ID: RDF.Auxilary.String_Holders.Holder) return Term_Type is
      use RDF.Auxilary.C_String_Holders;
      ID_N : C_String_Holder := To_C_String_Holder(ID);
   begin
      return From_Non_Null_Handle(C_Raptor_New_Term_From_Counted_Blank(Get_Handle(World), C_String(ID_N), Length(ID_N)));
   end;

   function C_Raptor_New_Term_From_Counted_Literal (World: RDF.Raptor.World.Handle_Type;
                                                    Literal: chars_ptr;
                                                    Literal_Len: size_t;
                                                    Datatype: RDF.Raptor.URI.Handle_Type;
                                                    Language: chars_ptr;
                                                    Language_Len: size_t)
                                                    return Term_Handle
      with Import, Convention=>C, External_Name=>"raptor_new_term_from_counted_literal";

   function From_Literal (World   : World_Type_Without_Finalize'Class;
                          Literal : RDF.Auxilary.String_Holders.Holder;
                          Datatype: RDF.Raptor.URI.URI_Type_Without_Finalize'Class;
                          Language: RDF.Auxilary.String_Holders.Holder)
                          return Term_Type
   is
      use RDF.Auxilary.C_String_Holders;
      Literal_N : C_String_Holder := To_C_String_Holder(Literal );
      Language_N: C_String_Holder := To_C_String_Holder(Language);
   begin
      return From_Non_Null_Handle( C_Raptor_New_Term_From_Counted_Literal(Get_Handle(World),
                                                                          C_String(Literal_N),
                                                                          Length(Literal_N),
                                                                          Get_Handle(Datatype),
                                                                          C_String(Language_N),
                                                                          Length(Language_N)) );
   end;

   function C_Raptor_New_Term_From_Counted_URI_String (World: RDF.Raptor.World.Handle_Type;
                                                       Str: char_array;
                                                       Str_Len: size_t)
                                                       return Term_Handle
      with Import, Convention=>C, External_Name=>"raptor_new_term_from_counted_uri_string";

   function From_URI_String (World: World_Type_Without_Finalize'Class; URI: String) return Term_Type is
   begin
      return From_Non_Null_Handle( C_Raptor_New_Term_From_Counted_URI_String(Get_Handle(World), To_C(URI, Append_Nul=>False), URI'Length) );
   end;

   function C_Raptor_New_Term_From_URI (World: RDF.Raptor.World.Handle_Type; URI: RDF.Raptor.URI.Handle_Type) return Term_Handle
      with Import, Convention=>C, External_Name=>"raptor_new_term_from_uri";

   function From_URI (World: World_Type_Without_Finalize'Class; URI: RDF.Raptor.URI.URI_Type_Without_Finalize'Class)
                      return Term_Type is
   begin
      return From_Non_Null_Handle( C_Raptor_New_Term_From_URI(Get_Handle(World), Get_Handle(URI)) );
   end;

   function C_Raptor_New_Term_From_Counted_String (World: RDF.Raptor.World.Handle_Type; Str: char_array; Len: size_t) return Term_Handle
      with Import, Convention=>C, External_Name=>"raptor_new_term_from_counted_string";

   function From_String (World: World_Type_Without_Finalize'Class; Value: String) return Term_Type is
   begin
      return From_Non_Null_Handle( C_Raptor_New_Term_From_Counted_String(Get_Handle(World), To_C(Value, Append_Nul=>False), Value'Length) );
   end;

   function C_Raptor_Term_Copy (Term: Term_Handle) return Term_Handle
      with Import, Convention=>C, External_Name=>"raptor_term_copy";

   procedure Adjust (Object: in out Term_Type) is
   begin
      Set_Handle_Hack(Object, C_Raptor_Term_Copy(Get_Handle(Object)));
   end;

   function Copy (Object: Term_Type_Without_Finalize'Class) return Term_Type_Without_Finalize is
   begin
      return From_Handle (C_Raptor_Term_Copy (Get_Handle(Object)) );
   end;

   function C_Raptor_Term_Compare (Left, Right: Term_Handle) return int
      with Import, Convention=>C, External_Name=>"raptor_term_compare";

   function Compare (Left, Right: Term_Type_Without_Finalize) return RDF.Auxilary.Comparison_Result is
   begin
      return RDF.Auxilary.Comparison_Result( C_Raptor_Term_Compare(Get_Handle(Left), Get_Handle(Right)) );
   end;

   function C_Raptor_Term_Equals (Left, Right: Term_Handle) return int
      with Import, Convention=>C, External_Name=>"raptor_term_equals";

   function Equals (Left, Right: Term_Type_Without_Finalize) return Boolean is
   begin
      return C_Raptor_Term_Equals(Get_Handle(Left), Get_Handle(Right)) /= 0;
   end;

   procedure C_Raptor_Free_Term (Term: Term_Handle)
      with Import, Convention=>C, External_Name=>"raptor_free_term";

   procedure Finalize_Handle (Object: Term_Type; Handle: Term_Handle) is
   begin
      C_Raptor_Free_Term(Handle);
   end;

   function C_Raptor_Term_To_String (Term: Term_Handle) return chars_ptr
      with Import, Convention=>C, External_Name=>"raptor_term_to_string";

   function To_String (Term: Term_Type_Without_Finalize) return String is
      Str: constant chars_ptr := C_Raptor_Term_To_String(Get_Handle(Term));
   begin
      if Str = Null_Ptr then
         raise Constraint_Error; -- TODO: Is it the best exception for the case?
      end if;
      declare
         Result: constant String := Value(Str);
      begin
         RDF.Raptor.Memory.Raptor_Free_Memory(Str);
         return Result;
      end;
   end;

   function C_Raptor_Term_To_Turtle_String (Term: Term_Handle;
                                            Stack: RDF.Raptor.Namespaces_Stacks.Namespace_Stack_Handle_Type;
                                            Base_URI: RDF.Raptor.URI.Handle_Type)
                                            return chars_ptr
      with Import, Convention=>C, External_Name=>"raptor_term_to_turtle_string";

   function To_Turtle_String(Term: Term_Type_Without_Finalize;
                             Stack: RDF.Raptor.Namespaces_Stacks.Namespace_Stack_Type'Class;
                             Base_URI: URI_Type'Class)
                             return String is
      Str: constant chars_ptr := C_Raptor_Term_To_Turtle_String(Get_Handle(Term), Get_Handle(Stack), Get_Handle(Base_URI));
   begin
      if Str = Null_Ptr then
         raise Constraint_Error; -- TODO: Is it the best exception for the case?
      end if;
      declare
         Result: constant String := Value(Str);
      begin
         RDF.Raptor.Memory.Raptor_Free_Memory(Str);
         return Result;
      end;
   end;

   function C_Raptor_Term_Turtle_Write (Stream: RDF.Raptor.IOStream.Handle_Type;
                                        Term: Term_Handle;
                                        Stack: RDF.Raptor.Namespaces_Stacks.Namespace_Stack_Handle_Type;
                                        Base_URI: RDF.Raptor.URI.Handle_Type)
                                        return Int
      with Import, Convention=>C, External_Name=>"raptor_term_turtle_write";

   procedure Turtle_Write (Stream: RDF.Raptor.IOStream.Stream_Type_Without_Finalize'Class;
                           Term: Term_Type_Without_Finalize;
                           Stack: RDF.Raptor.Namespaces_Stacks.Namespace_Stack_Type'Class;
                           Base_URI: URI_Type'Class) is
      use RDF.Raptor.IOStream;
   begin
      if C_Raptor_Term_Turtle_Write(Get_Handle(Stream), Get_Handle(Term), Get_Handle(Stack), Get_Handle(Base_URI)) /= 0 then
         raise Constraint_Error; -- TODO: Is it the best exception for the case?
      end if;
   end;

end RDF.Raptor.Term;
