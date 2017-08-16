with Interfaces.C; use Interfaces.C;

package body RDF.Rasqal.Literal is

   function rasqal_new_typed_literal (World: RDF.Rasqal.World.Handle_Type;
                                      Type_Of_Literal: Literal_Type_Enum;
                                      Value: char_array)
                                      return Literal_Handle_Type
     with Import, Convention=>C;


   function New_Typed_Literal (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                               Type_Of_Literal: Literal_Type_Enum;
                               Value: String)
                               return Literal_Type is
      use RDF.Rasqal.World;
   begin
      return From_Non_Null_Handle(rasqal_new_typed_literal(Get_Handle(World), Type_Of_Literal, To_C(Value)));
   end;

   function rasqal_new_boolean_literal (World: RDF.Rasqal.World.Handle_Type;
                                        Value: int)
                                        return Literal_Handle_Type
     with Import, Convention=>C;

   function From_Boolean (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                          Value: Boolean)
                          return Literal_Type is
      use RDF.Rasqal.World;
   begin
      return From_Non_Null_Handle(rasqal_new_boolean_literal(Get_Handle(World), Boolean'Pos(Value)));
   end;

   function rasqal_new_decimal_literal (World: RDF.Rasqal.World.Handle_Type;
                                        Value: char_array)
                                        return Literal_Handle_Type
     with Import, Convention=>C;

   function From_Decimal (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                          Value: String)
                          return Literal_Type is
      use RDF.Rasqal.World;
   begin
      return From_Non_Null_Handle(rasqal_new_decimal_literal(Get_Handle(World), To_C(Value)));
   end;

   function rasqal_new_double_literal (World: RDF.Rasqal.World.Handle_Type; Value: double)
                                       return Literal_Handle_Type
     with Import, Convention=>C;

   function rasqal_new_floating_literal (World: RDF.Rasqal.World.Handle_Type; Kind: Literal_Type_Enum; Value: double)
                                         return Literal_Handle_Type
     with Import, Convention=>C;

   function From_Float (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                        Value: Float)
                        return Literal_Type is
      use RDF.Rasqal.World;
   begin
      return From_Non_Null_Handle(rasqal_new_floating_literal(Get_Handle(World), Literal_Float, double(Value)));
   end;

   function From_Float (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                        Value: Long_Float)
                        return Literal_Type is
      use RDF.Rasqal.World;
   begin
      return From_Non_Null_Handle(rasqal_new_floating_literal(Get_Handle(World), Literal_Float, double(Value)));
   end;

   function From_Long_Float (World: RDF.Rasqal.World.World_Type_Without_Finalize;
                             Value: Long_Float)
                             return Literal_Type is
      use RDF.Rasqal.World;
   begin
      return From_Non_Null_Handle(rasqal_new_double_literal(Get_Handle(World), double(Value)));
   end;

end RDF.Rasqal.Literal;
