with Interfaces.C; use Interfaces.C;
with RDF.Auxiliary.Handled_Record;
with RDF.Rasqal.World;

package RDF.Rasqal.Literal is

   package Literal_Handled_Record is new RDF.Auxiliary.Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype Literal_Handle_Type is Literal_Handled_Record.Access_Type;

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

   type Literal_Type is new Literal_Type_Without_Finalize with null record;

   not overriding function New_Typed_Literal (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                                              Type_Of_Literal: Literal_Type_Enum;
                                              Value: String)
                                              return Literal_Type;

   not overriding function From_Boolean (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                                         Value: Boolean)
                                         return Literal_Type;

   -- Not implemented
--     not overriding function From_Datetime (World: RDF.Rasqal.World.World_Type_Without_Finalize;
--                                            Value: XSD_Datetime)
--                                            return Literal_Type;

   not overriding function From_Decimal (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                                         Value: String)
                                         return Literal_Type;

   -- Not implemented
--     not overriding function From_Decimal (World: RDF.Rasqal.World.World_Type_Without_Finalize;
--                                           Value: XSD_Decimal)
--                                           return Literal_Type;

   -- From_Float API is experimental

   not overriding function From_Float (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                                       Value: Float)
                                       return Literal_Type;

   -- WARNING: This takes a Long_Float value but silently rounds it to Float
   not overriding function From_Float (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                                       Value: Long_Float)
                                       return Literal_Type;

   not overriding function From_Long_Float (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                                            Value: Long_Float)
                                            return Literal_Type;

   -- Deliberately accept only long integers, don't implement "Value: int".
   not overriding function From_Integer (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                                         Value: long)
                                         return Literal_Type;

   -- TODO: Stopped at rasqal_new_pattern_literal ()

end RDF.Rasqal.Literal;
