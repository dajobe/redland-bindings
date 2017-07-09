with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary;
-- with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;

package body RDF.Raptor.Serializer is

   function raptor_serializer_start_to_iostream (Serializer: Handle_Type;
                                                 URI: RDF.Raptor.URI.Handle_Type;
                                                 Iostream: RDF.Raptor.Iostream.Handle_Type)
                                                 return int
      with Import, Convention=>C;

   procedure Start_To_Iostream (Serializer: Serializer_Type_Without_Finalize;
                                Iostream: RDF.Raptor.Iostream.Base_Stream_Type'Class;
                                URI: URI_Type_Without_Finalize := From_Handle(null)) is
      use RDF.Raptor.Iostream;
   begin
      if raptor_serializer_start_to_iostream(Get_Handle(Serializer), Get_Handle(URI), Get_Handle(Iostream)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

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

   procedure raptor_free_serializer (Serializer: Handle_Type)
      with Import, Convention=>C;

   procedure Finalize_Handle (Serializer: Serializer_Type; Handle: Handle_Type) is
   begin
      raptor_free_serializer(Handle);
   end;

end RDF.Raptor.Serializer;
