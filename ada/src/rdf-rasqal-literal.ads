with Interfaces.C; use Interfaces.C;
with RDF.Auxiliary.Handled_Record;
with RDF.Rasqal.World; use RDF.Rasqal.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;
with RDF.Auxiliary; use RDF.Auxiliary;
use RDF.Auxiliary.String_Holders;

package RDF.Rasqal.Literal is

   package Literal_Handled_Record is new RDF.Auxiliary.Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype Literal_Handle is Literal_Handled_Record.Access_Type;

   type Literal_Type_Without_Finalize is new Literal_Handled_Record.Base_Object with null record;

   type Literal_Type_Enum is (Literal_Unknown, -- internal
                              Literal_Blank,
                              Literal_URI,
                              Literal_String,
                              Literal_Xsd_String,
                              Literal_Boolean,
                              Literal_Integer,
                              Literal_Float,
                              Literal_Double,
                              Literal_Decimal,
                              Literal_Datetime,
                              Literal_Udt,
                              Literal_Pattern,
                              Literal_Qname,
                              Literal_Variable,
                              Literal_Date);

   subtype Literal_Type_Enum_Simple is Literal_Type_Enum
     with Static_Predicate => Literal_Type_Enum_Simple in Literal_Blank | Literal_Qname;

   overriding function "=" (Left, Right: Literal_Type_Without_Finalize) return Boolean;

   type Compare_Flags is mod 256; -- the number may change in a future version

   Compare_None:     constant Compare_Flags := 0;
   Compare_Nocase:   constant Compare_Flags := 1;
   Compare_XQuery:   constant Compare_Flags := 2;
   Compare_RDF:      constant Compare_Flags := 4;
   Compare_URI:      constant Compare_Flags := 8;
   Compare_Sameterm: constant Compare_Flags := 16;

   not overriding function As_String (Literal: Literal_Type_Without_Finalize;
                                      Flags: Compare_Flags := Compare_None)
                                      return String;

   not overriding function Compare (Left, Right: Literal_Type_Without_Finalize;
                                    Flags: Compare_Flags)
                                    return RDF.Auxiliary.Comparison_Result;

   not overriding function Get_Datatype (Literal: Literal_Type_Without_Finalize)
                                         return URI_Type_Without_Finalize;

   -- TODO: Not supported as of Rasqal 0.9.32
   --     not overriding function Get_Language (Literal: Literal_Type_Without_Finalize) return String_Holders.Holder;

   not overriding function Get_Rdf_Term_Type (Literal: Literal_Type_Without_Finalize)
                                              return Literal_Type_Enum;

   -- TODO: Not supported as of Rasqal 0.9.32
   --     not overriding function Get_Type (Literal: Literal_Type_Without_Finalize) return Literal_Type_Enum;

   not overriding function Is_Rdf_Literal (Literal: Literal_Type_Without_Finalize) return Boolean;

   not overriding procedure Print (Literal: Literal_Type_Without_Finalize;
                                   File: RDF.Auxiliary.C_File_Access);

   not overriding procedure Print_Type (Literal: Literal_Type_Without_Finalize;
                                        File: RDF.Auxiliary.C_File_Access);

   not overriding function Type_Label (Kind: Literal_Type_Enum) return String;

   type Literal_Type is new Literal_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Object: Literal_Type; Handle: Literal_Handle);

   not overriding function New_Typed_Literal (World: Rasqal_World_Type_Without_Finalize'Class;
                                              Type_Of_Literal: Literal_Type_Enum;
                                              Value: String)
                                              return Literal_Type;

   not overriding function From_Boolean (World: Rasqal_World_Type_Without_Finalize'Class;
                                         Value: Boolean)
                                         return Literal_Type;

   -- Not implemented
   --     not overriding function From_Datetime (World: Rasqal_World_Type_Without_Finalize;
   --                                            Value: XSD_Datetime)
   --                                            return Literal_Type;

   not overriding function From_Decimal (World: Rasqal_World_Type_Without_Finalize'Class;
                                         Value: String)
                                         return Literal_Type;

   -- Not implemented
   --     not overriding function From_Decimal (World: Rasqal_World_Type_Without_Finalize;
   --                                           Value: XSD_Decimal)
   --                                           return Literal_Type;

   -- From_Float API is experimental

   not overriding function From_Float (World: Rasqal_World_Type_Without_Finalize'Class;
                                       Value: Float)
                                       return Literal_Type;

   -- WARNING: This takes a Long_Float value but silently rounds it to Float
   not overriding function From_Float (World: Rasqal_World_Type_Without_Finalize'Class;
                                       Value: Long_Float)
                                       return Literal_Type;

   not overriding function From_Long_Float (World: Rasqal_World_Type_Without_Finalize'Class;
                                            Value: Long_Float)
                                            return Literal_Type;

   -- Deliberately accept only long integers, don't implement "Value: int".
   not overriding function From_Integer (World: Rasqal_World_Type_Without_Finalize'Class;
                                         Value: long)
                                         return Literal_Type;

   not overriding function New_Simple_Literal (World: Rasqal_World_Type_Without_Finalize'Class;
                                               Kind: Literal_Type_Enum_Simple;
                                               Value: String)
                                               return Literal_Type;

   -- overloaded
   not overriding function New_String_Literal (World: Rasqal_World_Type_Without_Finalize'Class;
                                               Value: String;
                                               Language: RDF.Auxiliary.String_Holders.Holder;
                                               Datatype: URI_Type_Without_Finalize'Class)
                                               return Literal_Type;

   not overriding function New_String_Literal (World: Rasqal_World_Type_Without_Finalize'Class;
                                               Value: String;
                                               Language: RDF.Auxiliary.String_Holders.Holder;
                                               Datatype_Qname: String)
                                               return Literal_Type;

   not overriding function New_String_Literal (World: Rasqal_World_Type_Without_Finalize'Class;
                                               Value: String;
                                               Language: RDF.Auxiliary.String_Holders.Holder := Empty_Holder)
                                               return Literal_Type;

   not overriding function From_String (World: Rasqal_World_Type_Without_Finalize'Class;
                                        Value: String)
                                        return Literal_Type
   is (New_String_Literal(World, Value));

   not overriding function From_URI (World: Rasqal_World_Type_Without_Finalize'Class;
                                     Value: URI_Type_Without_Finalize)
                                     return Literal_Type;

   not overriding function Value (Literal: Literal_Type_Without_Finalize'Class) return Literal_Type;

   not overriding function As_Node (Literal: Literal_Type_Without_Finalize'Class) return Literal_Type;

end RDF.Rasqal.Literal;
