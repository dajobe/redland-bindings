package body RDF.Base is

   function Get_Handle(Object: Base_Object) return Dummy_Record_Access is (Object.Handle);

   function From_Handle(Handle: Dummy_Record_Access) return Base_Object is
      (Ada.Finalization.Limited_Controlled with Handle=>Handle);

   function Default_Handle(Object: Base_Object) return Dummy_Record_Access is (null);

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

end RDF.Base;
