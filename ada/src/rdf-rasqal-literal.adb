with Ada.Unchecked_Conversion;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.C_String_Holders; use RDF.Auxiliary.C_String_Holders;
with RDF.Auxiliary.C_Pointers;
with RDF.Auxiliary.Convert;

package body RDF.Rasqal.Literal is

   function rasqal_new_typed_literal (World: Rasqal_World_Handle;
                                      Type_Of_Literal: Literal_Type_Enum;
                                      Value: char_array)
                                      return Literal_Handle
     with Import, Convention=>C;


   function New_Typed_Literal (World: Rasqal_World_Type_Without_Finalize'Class;
                               Type_Of_Literal: Literal_Type_Enum;
                               Value: String)
                               return Literal_Type is
      Handle: constant Literal_Handle :=
        rasqal_new_typed_literal(Get_Handle(World), Type_Of_Literal, To_C(Value));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function rasqal_new_boolean_literal (World: Rasqal_World_Handle;
                                        Value: int)
                                        return Literal_Handle
     with Import, Convention=>C;

   function From_Boolean (World: Rasqal_World_Type_Without_Finalize'Class;
                          Value: Boolean)
                          return Literal_Type is
   begin
      return From_Non_Null_Handle(rasqal_new_boolean_literal(Get_Handle(World), Boolean'Pos(Value)));
   end;

   function rasqal_new_decimal_literal (World: Rasqal_World_Handle;
                                        Value: char_array)
                                        return Literal_Handle
     with Import, Convention=>C;

   function From_Decimal (World: Rasqal_World_Type_Without_Finalize'Class;
                          Value: String)
                          return Literal_Type is
   begin
      return From_Non_Null_Handle(rasqal_new_decimal_literal(Get_Handle(World), To_C(Value)));
   end;

   function rasqal_new_double_literal (World: Rasqal_World_Handle; Value: double)
                                       return Literal_Handle
     with Import, Convention=>C;

   function rasqal_new_floating_literal (World: Rasqal_World_Handle; Kind: Literal_Type_Enum; Value: double)
                                         return Literal_Handle
     with Import, Convention=>C;

   function From_Float (World: Rasqal_World_Type_Without_Finalize'Class;
                        Value: Float)
                        return Literal_Type is
   begin
      return From_Non_Null_Handle(rasqal_new_floating_literal(Get_Handle(World), Literal_Float, double(Value)));
   end;

   function From_Float (World: Rasqal_World_Type_Without_Finalize'Class;
                        Value: Long_Float)
                        return Literal_Type is
   begin
      return From_Non_Null_Handle(rasqal_new_floating_literal(Get_Handle(World), Literal_Float, double(Value)));
   end;

   function From_Long_Float (World: Rasqal_World_Type_Without_Finalize'Class;
                             Value: Long_Float)
                             return Literal_Type is
   begin
      return From_Non_Null_Handle(rasqal_new_double_literal(Get_Handle(World), double(Value)));
   end;

   function rasqal_new_numeric_literal_from_long (World: Rasqal_World_Handle;
                                                  Kind: Literal_Type_Enum;
                                                  Value: Long)
                                                  return Literal_Handle
     with Import, Convention=>C;

   function From_Integer (World: Rasqal_World_Type_Without_Finalize'Class; Value: long)
                          return Literal_Type is
   begin
      return From_Non_Null_Handle(rasqal_new_numeric_literal_from_long(Get_Handle(World), Literal_Integer, Value));
   end;

   function rasqal_new_simple_literal (World: Rasqal_World_Handle;
                                       Kind: Literal_Type_Enum_Simple;
                                       Value: chars_ptr)
                                       return Literal_Handle
     with Import, Convention=>C;

   function New_Simple_Literal (World: Rasqal_World_Type_Without_Finalize'Class;
                                Kind: Literal_Type_Enum_Simple;
                                Value: String)
                                return Literal_Type is
      Value2: constant chars_ptr := New_String(Value); -- freed by rasqal_new_simple_literal
   begin
      return From_Non_Null_Handle(rasqal_new_simple_literal(Get_Handle(World), Kind, Value2));
   end;

   function rasqal_new_string_literal (World: Rasqal_World_Handle;
                                       Value: chars_ptr;
                                       Language: chars_ptr;
                                       Datatype: URI_Handle;
                                       Datatype_Qname: chars_ptr)
                                       return Literal_Handle
     with Import, Convention=>C;

   function New_String_Literal (World: Rasqal_World_Type_Without_Finalize'Class;
                                Value: String;
                                Language: RDF.Auxiliary.String_Holders.Holder;
                                Datatype: URI_Type_Without_Finalize'Class)
                                return Literal_Type is
      Handle: constant Literal_Handle :=
        rasqal_new_string_literal(Get_Handle(World),
                                  New_String(Value),
                                  New_String(Language),
                                  Get_Handle(Datatype),
                                  Null_Ptr);
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function New_String_Literal (World: Rasqal_World_Type_Without_Finalize'Class;
                                Value: String;
                                Language: RDF.Auxiliary.String_Holders.Holder;
                                Datatype_Qname: String)
                                return Literal_Type is
      Handle: constant Literal_Handle :=
        rasqal_new_string_literal(Get_Handle(World),
                                  New_String(Value),
                                  New_String(Language),
                                  null,
                                  New_String(Datatype_Qname));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function New_String_Literal (World: Rasqal_World_Type_Without_Finalize'Class;
                                Value: String;
                                Language: RDF.Auxiliary.String_Holders.Holder := Empty_Holder)
                                return Literal_Type is
   begin
      return From_Non_Null_Handle(rasqal_new_string_literal(
                                  Get_Handle(World),
                                  New_String(Value),
                                  New_String(Language),
                                  null,
                                  Null_Ptr));
   end;

   function rasqal_new_uri_literal (World: Rasqal_World_Handle;
                                    Value: URI_Handle)
                                    return Literal_Handle
     with Import, Convention=>C;

   function From_URI (World: Rasqal_World_Type_Without_Finalize'Class;
                      Value: URI_Type_Without_Finalize)
                      return Literal_Type is
   begin
      return From_Non_Null_Handle(rasqal_new_uri_literal(Get_Handle(World), Get_Handle(Value)));
   end;

   function rasqal_literal_same_term (Left, Right: Literal_Handle) return int
     with Import, Convention=>C;

   function "=" (Left, Right: Literal_Type_Without_Finalize) return Boolean is
     (rasqal_literal_same_term(Get_Handle(Left), Get_Handle(Right)) /= 0);

   procedure rasqal_free_literal (Handle: Literal_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Literal_Type_Without_Finalize; Handle: Literal_Handle) is
   begin
      rasqal_free_literal(Handle);
   end;

   function rasqal_literal_as_node (Literal: Literal_Handle) return Literal_Handle
     with Import, Convention=>C;

   function As_Node (Literal: Literal_Type_Without_Finalize'Class) return Literal_Type is
     (From_Handle(rasqal_literal_as_node(Get_Handle(Literal))));

   type Size_T_P is access all size_t with Convention=>C;
   type Int_P is access all int with Convention=>C;

   function rasqal_literal_as_counted_string (Literal: Literal_Handle;
                                              Len_P: Size_T_P;
                                              Flags: int;
                                              Error_P: Int_P)
                                              return RDF.Auxiliary.C_Pointers.Pointer
     with Import, Convention=>C;

   function As_String (Literal: Literal_Type_Without_Finalize; Flags: Compare_Flags := Compare_None)
                       return String is
      Error: aliased Int := 0;
      Length: aliased size_t;
      Item: constant RDF.Auxiliary.C_Pointers.Pointer :=
        rasqal_literal_as_counted_string(Get_Handle(Literal),
                                         Length'Unchecked_Access,
                                         int(Flags),
                                         Error'Unchecked_Access);
      use RDF.Auxiliary.Convert;
   begin
      if Error /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
      return Value_With_Possible_NULs(Item, Length);
   end;

   function rasqal_literal_compare (Left, Right: Literal_Handle; Flags: int; Error_P: Int_P)
                                    return int
     with Import, Convention=>C;

   function Compare (Left, Right: Literal_Type_Without_Finalize; Flags: Compare_Flags)
                     return RDF.Auxiliary.Comparison_Result is
      Error: aliased Int := 0;
   begin
      return Sign(rasqal_literal_compare(Get_Handle(Left), Get_Handle(Right), int(Flags), Error'Unchecked_Access));
   end;

   function rasqal_literal_datatype (Literal: Literal_Handle) return URI_Handle
     with Import, Convention=>C;

   function Get_Datatype (Literal: Literal_Type_Without_Finalize)
                          return URI_Type_Without_Finalize is
   begin
      return From_Handle(rasqal_literal_datatype(Get_Handle(Literal)));
   end;

   -- TODO: Not supported as of Rasqal 0.9.32
   --     function rasqal_literal_get_language (Literal: Literal_Handle) return Chars_Ptr
   --       with Import, Convention=>C;
   --
   --     function Get_Language (Literal: Literal_Type_Without_Finalize) return String_Holders.Holder is
   --     begin
   --        return New_Holder(rasqal_literal_get_language(Get_Handle(Literal)));
   --     end;

   function rasqal_literal_get_rdf_term_type (Literal: Literal_Handle) return Literal_Type_Enum
     with Import, Convention=>C;

   --     function rasqal_literal_get_type (Literal: Literal_Handle) return Literal_Type_Enum
   --       with Import, Convention=>C;

   function Get_Rdf_Term_Type (Literal: Literal_Type_Without_Finalize) return Literal_Type_Enum is
     (rasqal_literal_get_rdf_term_type(Get_Handle(Literal)));

   --     function Get_Type (Literal: Literal_Type_Without_Finalize) return Literal_Type_Enum is
   --       (rasqal_literal_get_type(Get_Handle(Literal)));

   function rasqal_literal_is_rdf_literal (Literal: Literal_Handle) return int
     with Import, Convention=>C;

   function Is_Rdf_Literal (Literal: Literal_Type_Without_Finalize) return Boolean is
     (rasqal_literal_is_rdf_literal(Get_Handle(Literal)) /= 0);

   function rasqal_literal_print (Literal: Literal_Handle; File: RDF.Auxiliary.C_File_Access) return int
     with Import, Convention=>C;

   procedure Print (Literal: Literal_Type_Without_Finalize; File: RDF.Auxiliary.C_File_Access) is
   begin
      if rasqal_literal_print (Get_Handle(Literal), File) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

   procedure rasqal_literal_print_type (Literal: Literal_Handle; File: RDF.Auxiliary.C_File_Access)
     with Import, Convention=>C;

   procedure Print_Type (Literal: Literal_Type_Without_Finalize; File: RDF.Auxiliary.C_File_Access) is
   begin
      rasqal_literal_print_type (Get_Handle(Literal), File);
   end;

   function rasqal_literal_type_label (Kind: Literal_Type_Enum) return chars_ptr
     with Import, Convention=>C;

   function Type_Label (Kind: Literal_Type_Enum) return String is
     (Value(rasqal_literal_type_label(Kind)));

   function rasqal_literal_value (Literal: Literal_Handle) return Literal_Handle
     with Import, Convention=>C;

   function Value (Literal: Literal_Type_Without_Finalize'Class) return Literal_Type is
     (From_Handle(rasqal_literal_value(Get_Handle(Literal))));

end RDF.Rasqal.Literal;
