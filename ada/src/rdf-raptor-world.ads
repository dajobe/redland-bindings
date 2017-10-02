with RDF.Auxiliary; use RDF.Auxiliary;
with RDF.Auxiliary.Limited_Handled_Record;

package RDF.Raptor.World is

   package Raptor_World_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Raptor_World_Type_Without_Finalize is new Raptor_World_Handled_Record.Base_Object with null record;

   subtype Raptor_World_Handle is Raptor_World_Handled_Record.Access_Type;

   type Raptor_Flag_Type is (Libxml_Error_Save,
                      Libxml_Structured_Error_Save,
                      URI_Interning,
                      WWW_Skip_Init_Finish)
      with Convention => C;
   for Raptor_Flag_Type use (Libxml_Error_Save => 1,
                      Libxml_Structured_Error_Save => 2,
                      URI_Interning => 3,
                      WWW_Skip_Init_Finish => 4);

   type Flag_And_Value is
      record
         Flag : Raptor_Flag_Type;
         Value: Boolean;
      end record;

   type Flags_Array is array(Integer range <>) of Flag_And_Value;

   overriding function Default_Handle(Object: Raptor_World_Type_Without_Finalize) return Raptor_World_Handle;

   not overriding procedure Open(Object: in out Raptor_World_Type_Without_Finalize);

   not overriding procedure Open(Object: in out Raptor_World_Type_Without_Finalize; Flags: Flags_Array);

   not overriding procedure Set_Flag(Object: in out Raptor_World_Type_Without_Finalize; Flag: Raptor_Flag_Type; Value: Boolean);

   not overriding procedure Set_Flags(Object: in out Raptor_World_Type_Without_Finalize; Flags: Flags_Array);

   type Raptor_World_Type is new Raptor_World_Type_Without_Finalize with null record;

   not overriding function Open return Raptor_World_Type;

   not overriding function Open(Flags: Flags_Array) return Raptor_World_Type;

   overriding procedure Finalize_Handle(Object: Raptor_World_Type; Handle: Raptor_World_Handle);

   -- Not implemented
   -- procedure Set_Libxslt_Security_Preferences

end RDF.Raptor.World;
