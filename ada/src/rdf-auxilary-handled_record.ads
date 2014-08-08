with Ada.Finalization;

generic
   type Record_Type; -- It should be an untagged record for C compatibility
package RDF.Auxilary.Handled_Record is

   type Access_Type is access Record_Type
      with Convention=>C;

   -- It is logically abstract, but not exactly abstract in Ada sense.
   -- It can't be abstract because the function From_Handle returns this type.
   --
   -- When extending this type, it is recommended:
   -- 1. Create a type T_Without_Finalize for which do NOT override Finalize_Handle and Adjust.
   -- 2. Create T extending T_Without_Finalize for which do override Finalize_Handle and Adjust.
   -- 3. Routines returning newly created objects of this type should return T.
   -- 4. The rest routines should operate on T or T_Without_Finalize'Class, as appropriate.
   --
   -- Note: T_Without_Finalize is useful for objects which are logically a part of on other object
   -- and thus should be finalized only as a part of finalization of the container.
   type Base_Object is new Ada.Finalization.Controlled with private;

   overriding procedure Initialize(Object: in out Base_Object);

   overriding procedure Finalize(Object: in out Base_Object);

--     overriding procedure Adjust(Object: in out Base_Object);

   -- Don't call this procedure unless you really need it.
   not overriding procedure Set_Handle_Hack(Object: in out Base_Object; Handle: Access_Type);

   -- TODO: Should check non-null predicate?
   not overriding function Get_Handle(Object: Base_Object) return Access_Type with Inline;

   not overriding function From_Handle(Handle: Access_Type) return Base_Object with Inline;

   not overriding function Default_Handle(Object: Base_Object) return Access_Type;

   not overriding procedure Finalize_Handle(Object: Base_Object; Handle: Access_Type) is null;

--     not overriding function Copy_Handle(Object: Base_Object; Handle: Access_Type) return Access_Type;

private

   type Base_Object is new Ada.Finalization.Controlled with
      record
         Handle: Access_Type;
      end record;

end RDF.Auxilary.Handled_Record;
