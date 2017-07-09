with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Raptor.Constants;
with RDF.Raptor.Memory;
with RDF.Auxiliary.C_String_Holders; use RDF.Auxiliary.C_String_Holders;

package body RDF.Raptor.World is

   function raptor_new_world_internal(Version: Interfaces.C.unsigned) return Handle_Type
     with Import, Convention=>C;

   procedure raptor_world_open(Handle: Handle_Type)
     with Import, Convention=>C;

   procedure raptor_world_set_flag(Handle: Handle_Type; Flag: Flag_Type; Value: Interfaces.C.int)
     with Import, Convention=>C;

   function Default_Handle(Object: World_Type_Without_Finalize) return Handle_Type is
   begin
      return raptor_new_world_internal(RDF.Raptor.Constants.version_decimal);
   end;

   procedure Open(Object: World_Type_Without_Finalize) is
   begin
      raptor_world_open(Get_Handle(Object));
   end;

   procedure Open(Object: World_Type_Without_Finalize; Flags: Flags_Array) is
   begin
      Set_Flags(Object, Flags);
      Open(Object);
   end;

   procedure Set_Flag(Object: World_Type_Without_Finalize; Flag: Flag_Type; Value: Boolean) is
   begin
      raptor_world_set_flag(Get_Handle(Object), Flag, (if Value then 1 else 0));
   end;

   procedure Set_Flags(Object: World_Type_Without_Finalize; Flags: Flags_Array) is
   begin
      for Element of Flags loop
         Set_Flag(Object, Element.Flag, Element.Value);
      end loop;
   end;

   function Open return World_Type is
      Object: World_Type;
   begin
      return Object: World_Type do
         Open(Object);
      end return;
   end;

   function Open(Flags: Flags_Array) return World_Type is
      Object: World_Type;
   begin
      return Object: World_Type do
         Open(Object, Flags);
      end return;
   end;

   procedure raptor_free_world(Handle: Handle_Type)
     with Import, Convention=>C;

   procedure Finalize_Handle(Object: World_Type; Handle: Handle_Type) is
   begin
      raptor_free_world(Handle);
   end;

end RDF.Raptor.World;
