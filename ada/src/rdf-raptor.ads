with Interfaces.C;
with RDF.Base;

package RDF.Raptor is

   type World is new RDF.Base.Base_Object with null record;

   type Flag_Type is (Libxml_Error_Save,
                      Libxml_Structured_Error_Save,
		      URI_Interning,
                      WWW_Skip_Init_Finish);
   for Flag_Type'Size use Interfaces.C.int'Size; -- hack
   for Flag_Type use (Libxml_Error_Save => 1,
                      Libxml_Structured_Error_Save => 2,
		      URI_Interning => 3,
                      WWW_Skip_Init_Finish => 4);

   type Flag_And_Value is
      record
         Flag : Flag_Type;
         Value: Boolean;
      end record;

   type Flags_Array is array(Integer range <>) of Flag_And_Value;

   overriding function Default_Handle(Object: World) return RDF.Base.Dummy_Record_Access;

   not overriding procedure Open(Object: World);

   not overriding procedure Open(Object: World; Flags: Flags_Array);

   not overriding function Open return World;

   not overriding function Open(Flags: Flags_Array) return World;

   overriding procedure Finalize_Handle(Object: World; Handle: RDF.Base.Dummy_Record_Access);

   not overriding procedure Set_Flag(Object: World; Flag: Flag_Type; Value: Boolean);

   not overriding procedure Set_Flags(Object: World; Flags: Flags_Array);

   --overriding function From_Handle(Handle: RDF.Base.Dummy_Record_Access) return World with Inline;

end;
