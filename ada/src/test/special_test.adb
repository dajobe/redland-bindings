-- In an unknown reason this program prints wrong value 0 (NULL pointer) as its output.
-- Please help to find the error - porton@narod.ru

with Ada.Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with RDF.Raptor.World; use RDF.Raptor.World;
with RDF.Raptor.URI; use RDF.Raptor.URI;

procedure Special_Test is

   function C_Raptor_New_World_Internal(Version: Interfaces.C.unsigned) return RDF.Raptor.World.Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_world_internal";

   procedure C_Raptor_Free_World(Handle: RDF.Raptor.World.Handle_Type)
     with Import, Convention=>C, External_Name=>"raptor_free_world";

   function C_Raptor_New_Uri_From_Counted_String (World_Handle: RDF.Raptor.World.Handle_Type;
                                                  URI_String: char_array;
                                                  Length    : Interfaces.C.size_t)
                                                  return RDF.Raptor.URI.Handle_Type
     with Import, Convention=>C, External_Name=>"raptor_new_uri_from_counted_string";

   function My_Conv is new Ada.Unchecked_Conversion(RDF.Raptor.URI.Handle_Type, Integer_Address);

   World: RDF.Raptor.World.World_Type;
   URI_1: constant String := "http://example.org/xyz";
   URI_1_Handle: constant RDF.Raptor.URI.Handle_Type := Get_Handle(From_String(World, URI_1));
--     URI_1_Handle: constant RDF.Raptor.URI.Handle_Type :=
--       C_Raptor_New_Uri_From_Counted_String(Get_Handle(World), To_C(URI_1, Append_Nul=>True), URI_1'Length);

begin
   Ada.Text_IO.Put_Line( Integer_Address'Image (My_Conv(URI_1_Handle)));
end;
