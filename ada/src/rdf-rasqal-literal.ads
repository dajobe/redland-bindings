with RDF.Auxiliary.Handled_Record;

package RDF.Rasqal.Literal is

   package Literal_Handled_Record is new RDF.Auxiliary.Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   subtype Literal_Type is Literal_Handled_Record.Access_Type;

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

   -- TODO: Stopped at rasqal_new_typed_literal ()

end RDF.Rasqal.Literal;
