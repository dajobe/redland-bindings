with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
-- with RDF.Raptor.World; use RDF.Raptor.World;

package body RDF.Raptor.Serializer is

   function raptor_new_serializer (World: RDF.Raptor.World.Handle_Type; Syntax_Name: chars_ptr) return Handle_Type
      with Import, Convention=>C;

   function New_Serializer (World: World_Type) return Serializer_Type is
   begin
      return From_Non_Null_Handle(raptor_new_serializer(Get_Handle(World), Null_Ptr));
   end;

   function New_Serializer (World: World_Type; Syntax_Name: String) return Serializer_Type is
      V: aliased char_array := To_C(Syntax_Name);
   begin
      return From_Non_Null_Handle(raptor_new_serializer(Get_Handle(World), To_Chars_Ptr(V'Unchecked_Access)));
   end;

end RDF.Raptor.Serializer;
