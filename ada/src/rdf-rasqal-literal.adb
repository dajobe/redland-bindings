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
   end New_Typed_Literal;

end RDF.Rasqal.Literal;
