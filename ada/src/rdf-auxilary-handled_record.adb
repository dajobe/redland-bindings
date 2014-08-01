package body RDF.Auxilary.Handled_Record is

   procedure Set_Handle_Hack(Object: in out Base_Object; Handle: Access_Type) is
   begin
      Object.Handle := Handle;
   end;

   function Get_Handle(Object: Base_Object) return Access_Type is (Object.Handle);

   function From_Handle(Handle: Access_Type) return Base_Object is
      (Ada.Finalization.Controlled with Handle=>Handle);

   function Default_Handle(Object: Base_Object) return Access_Type is (null);

   procedure Initialize (Object: in out Base_Object) is
   begin
      Object.Handle := Default_Handle(Base_Object'Class(Object));
   end Initialize;

   procedure Finalize (Object : in out Base_Object) is
   begin
      if Object.Handle /= null then
         Finalize_Handle(Base_Object'Class(Object), Object.Handle);
	 Object.Handle := null;
      end if;
   end Finalize;

   procedure Adjust(Object: in out Base_Object) is
   begin
      if Object.Handle /= null then
         Object.Handle := Copy_Handle(Object, Object.Handle);
      end if;
   end;

end RDF.Auxilary.Handled_Record;
