with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.C_String_Holders;
with RDF.Raptor.Memory;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Raptor.Namespace; use RDF.Raptor.Namespace;
with RDF.Raptor.Namespace_Stack; use RDF.Raptor.Namespace_Stack;
with RDF.Auxiliary.Convert; use RDF.Auxiliary.Convert;

package body RDF.Raptor.Term is

   function Get_World (Term: Term_Type_Without_Finalize) return Raptor_World_Type_Without_Finalize is
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

   function Datatype (Literal: Term_Literal_Value) return URI_Type_Without_Finalize is
      use type URI_Handle;
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

   function raptor_new_term_from_counted_blank (World: Raptor_World_Handle;
                                                  Blank: chars_ptr;
                                                  Length: size_t)
                                                  return Term_Handle
     with Import, Convention=>C;

   function From_Blank (World: Raptor_World_Type_Without_Finalize'Class) return Term_Type is
      use RDF.Raptor.World;
   begin
      return From_Non_Null_Handle(raptor_new_term_from_counted_blank(Get_Handle(World), Null_Ptr, 0));
   end;

   function From_Blank (World: Raptor_World_Type_Without_Finalize'Class; ID: String) return Term_Type is
      use RDF.Raptor.World;
      Str: aliased char_array := To_C(ID);
   begin
      return From_Non_Null_Handle(raptor_new_term_from_counted_blank(Get_Handle(World), To_Chars_Ptr(Str'Unchecked_Access), ID'Length));
   end;

   function From_Blank (World: Raptor_World_Type_Without_Finalize'Class; ID: RDF.Auxiliary.String_Holders.Holder) return Term_Type is
      use RDF.Auxiliary.C_String_Holders;
      ID_N : C_String_Holder := To_C_String_Holder(ID);
   begin
      return From_Non_Null_Handle(raptor_new_term_from_counted_blank(Get_Handle(World), C_String(ID_N), Length(ID_N)));
   end;

   function raptor_new_term_from_counted_literal (World: Raptor_World_Handle;
                                                    Literal: chars_ptr;
                                                    Literal_Len: size_t;
                                                    Datatype: URI_Handle;
                                                    Language: chars_ptr;
                                                    Language_Len: size_t)
                                                    return Term_Handle
      with Import, Convention=>C;

   function From_Literal (World   : Raptor_World_Type_Without_Finalize'Class;
                          Literal : RDF.Auxiliary.String_Holders.Holder;
                          Datatype: URI_Type_Without_Finalize'Class;
                          Language: RDF.Auxiliary.String_Holders.Holder)
                          return Term_Type
   is
      use RDF.Auxiliary.C_String_Holders;
      Literal_N : C_String_Holder := To_C_String_Holder(Literal );
      Language_N: C_String_Holder := To_C_String_Holder(Language);
   begin
      return From_Non_Null_Handle( raptor_new_term_from_counted_literal(Get_Handle(World),
                                                                          C_String(Literal_N),
                                                                          Length(Literal_N),
                                                                          Get_Handle(Datatype),
                                                                          C_String(Language_N),
                                                                          Length(Language_N)) );
   end;

   function raptor_new_term_from_counted_uri_string (World: Raptor_World_Handle;
                                                       Str: char_array;
                                                       Str_Len: size_t)
                                                       return Term_Handle
      with Import, Convention=>C;

   function From_URI_String (World: Raptor_World_Type_Without_Finalize'Class; URI: URI_String) return Term_Type is
   begin
      return From_Non_Null_Handle( raptor_new_term_from_counted_uri_string(Get_Handle(World), My_To_C_Without_Nul(String(URI)), URI'Length) );
   end;

   function raptor_new_term_from_uri (World: Raptor_World_Handle; URI: URI_Handle) return Term_Handle
      with Import, Convention=>C;

   function From_URI (World: Raptor_World_Type_Without_Finalize'Class; URI: URI_Type_Without_Finalize'Class)
                      return Term_Type is
   begin
      return From_Non_Null_Handle( raptor_new_term_from_uri(Get_Handle(World), Get_Handle(URI)) );
   end;

   function raptor_new_term_from_counted_string (World: Raptor_World_Handle; Str: char_array; Len: size_t) return Term_Handle
      with Import, Convention=>C;

   function From_String (World: Raptor_World_Type_Without_Finalize'Class; Value: String) return Term_Type is
   begin
      return From_Non_Null_Handle( raptor_new_term_from_counted_string(Get_Handle(World), My_To_C_Without_Nul(Value), Value'Length) );
   end;

   function raptor_term_copy (Term: Term_Handle) return Term_Handle
      with Import, Convention=>C;

   procedure Adjust (Object: in out Term_Type) is
   begin
      if Get_Handle(Object) /= null then
         Set_Handle_Hack(Object, Raptor_Term_Copy(Get_Handle(Object)));
      end if;
   end;

   function Copy (Object: Term_Type_Without_Finalize'Class) return Term_Type_Without_Finalize is
   begin
      return From_Handle (raptor_term_copy (Get_Handle(Object)) );
   end;

   function raptor_term_compare (Left, Right: Term_Handle) return int
      with Import, Convention=>C;

   function Compare (Left, Right: Term_Type_Without_Finalize) return RDF.Auxiliary.Comparison_Result is
   begin
      return RDF.Auxiliary.Sign( raptor_term_compare(Get_Handle(Left), Get_Handle(Right)) );
   end;

   function raptor_term_equals (Left, Right: Term_Handle) return int
      with Import, Convention=>C;

   function Equals (Left, Right: Term_Type_Without_Finalize) return Boolean is
   begin
      return raptor_term_equals(Get_Handle(Left), Get_Handle(Right)) /= 0;
   end;

   procedure raptor_free_term (Term: Term_Handle)
      with Import, Convention=>C;

   procedure Finalize_Handle (Object: Term_Type; Handle: Term_Handle) is
   begin
      raptor_free_term(Handle);
   end;

   function raptor_term_to_string (Term: Term_Handle) return chars_ptr
      with Import, Convention=>C;

   function To_String (Term: Term_Type_Without_Finalize) return String is
      Str: constant chars_ptr := raptor_term_to_string(Get_Handle(Term));
   begin
      if Str = Null_Ptr then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      declare
         Result: constant String := Value(Str);
      begin
         RDF.Raptor.Memory.Raptor_Free_Memory(Str);
         return Result;
      end;
   end;

   function raptor_term_to_turtle_string (Term: Term_Handle;
                                            Stack: Namespace_Stack_Handle;
                                            Base_URI: URI_Handle)
                                            return chars_ptr
      with Import, Convention=>C;

   function To_Turtle_String(Term: Term_Type_Without_Finalize;
                             Stack: Namespace_Stack_Type'Class;
                             Base_URI: URI_Type'Class)
                             return String is
      Str: constant chars_ptr := raptor_term_to_turtle_string(Get_Handle(Term), Get_Handle(Stack), Get_Handle(Base_URI));
   begin
      if Str = Null_Ptr then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      declare
         Result: constant String := Value(Str);
      begin
         RDF.Raptor.Memory.Raptor_Free_Memory(Str);
         return Result;
      end;
   end;

   function raptor_term_turtle_write (Stream: IOStream_Handle;
                                        Term: Term_Handle;
                                        Stack: Namespace_Stack_Handle;
                                        Base_URI: URI_Handle)
                                        return Int
      with Import, Convention=>C;

   procedure Turtle_Write (Stream: Base_Stream_Type'Class;
                           Term: Term_Type_Without_Finalize;
                           Stack: Namespace_Stack_Type'Class;
                           Base_URI: URI_Type'Class) is
      use RDF.Raptor.IOStream;
   begin
      if raptor_term_turtle_write(Get_Handle(Stream), Get_Handle(Term), Get_Handle(Stack), Get_Handle(Base_URI)) /= 0 then
         raise IOStream_Exception;
      end if;
   end;

end RDF.Raptor.Term;
