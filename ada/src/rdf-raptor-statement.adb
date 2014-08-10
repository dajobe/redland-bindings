with RDF.Auxilary.C_String_Holders;

package body RDF.Raptor.Statement is

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
      return From_Handle(C_Raptor_New_Term_From_Counted_Blank(Get_Handle(World), Null_Ptr, 0));
   end;

   function From_Blank (World: World_Type_Without_Finalize'Class; ID: String) return Term_Type is
      use RDF.Raptor.World;
      Str: aliased char_array := To_C(ID);
   begin
      return From_Handle(C_Raptor_New_Term_From_Counted_Blank(Get_Handle(World), To_Chars_Ptr(Str'Unchecked_Access), ID'Length));
   end;

   -- TODO: Uncomment
--     function From_Blank (World: World_Type_Without_Finalize'Class; ID: RDF.Auxilary.String_Holders.Holder) return Term_Type is
--        use RDF.Auxilary.String_Holders;
--     begin
--        if Is_Empty(ID) then
--           return From_Handle(C_Raptor_New_Term_From_Counted_Blank(Get_Handle(World), Null_Ptr, 0));
--        else
--     end;

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
      return From_Handle( C_Raptor_New_Term_From_Counted_Literal(Get_Handle(World),
                                                                 C_String(Literal_N),
                                                                 Length(Literal_N),
                                                                 Get_Handle(Datatype),
                                                                 C_String(Language_N),
                                                                 Length(Language_N)) );
   end;

end RDF.Raptor.Statement;
