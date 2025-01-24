with Interfaces.C; use Interfaces.C;
with RDF.Raptor.Constants;

package body RDF.Raptor.World is

   function raptor_new_world_internal(Version: Interfaces.C.unsigned) return Raptor_World_Handle
     with Import, Convention=>C;

   procedure raptor_world_open(Handle: Raptor_World_Handle)
     with Import, Convention=>C;

   procedure raptor_world_set_flag(Handle: Raptor_World_Handle; Flag: Raptor_Flag_Type; Value: Interfaces.C.int)
     with Import, Convention=>C;

   function Default_Handle(Object: Raptor_World_Type_Without_Finalize) return Raptor_World_Handle is
   begin
      return raptor_new_world_internal(RDF.Raptor.Constants.version_decimal);
   end;

   procedure Open(Object: in out Raptor_World_Type_Without_Finalize) is
   begin
      raptor_world_open(Get_Handle(Object));
   end;

   procedure Open(Object: in out Raptor_World_Type_Without_Finalize; Flags: Flags_Array) is
   begin
      Set_Flags(Object, Flags);
      Open(Object);
   end;

   procedure Set_Flag(Object: in out Raptor_World_Type_Without_Finalize;
                      Flag: Raptor_Flag_Type;
                      Value: Boolean) is
   begin
      raptor_world_set_flag(Get_Handle(Object), Flag, (if Value then 1 else 0));
   end;

   procedure Set_Flags(Object: in out Raptor_World_Type_Without_Finalize; Flags: Flags_Array) is
   begin
      for Element of Flags loop
         Set_Flag(Object, Element.Flag, Element.Value);
      end loop;
   end;

   function Open return Raptor_World_Type is
   begin
      return Object: Raptor_World_Type do
         Open(Object);
      end return;
   end;

   function Open(Flags: Flags_Array) return Raptor_World_Type is
   begin
      return Object: Raptor_World_Type do
         Open(Object, Flags);
      end return;
   end;

   procedure raptor_free_world(Handle: Raptor_World_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Raptor_World_Type_Without_Finalize; Handle: Raptor_World_Handle) is
   begin
      raptor_free_world(Handle);
   end;

end RDF.Raptor.World;
