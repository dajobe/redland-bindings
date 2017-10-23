with Ada.Finalization;

generic
   type Record_Type; -- It should be a convention C record
   type Record_Type_Access is access Record_Type;  -- It should be a convention C access type
package RDF.Auxiliary.Handled_Record is

   -- Does not compile because https://gcc.gnu.org/bugzilla/show_bug.cgi?id=62235
   --    subtype Access_Type is access Record_Type
   --       with Convention=>C;

   subtype Access_Type is Record_Type_Access;

--     type Finalization_Procedures is interface;
--
--     procedure Do_Finalize(Object: in out Finalization_Procedures) is null;
--     procedure Do_Adjust(Object: in out Finalization_Procedures) is null;

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

   procedure Do_Finalize(Object: in out Base_Object);

   procedure Do_Adjust(Object: in out Base_Object);

   -- Don't call this procedure unless you really need it.
   -- TODO: Do we need this procedure at all?
   not overriding procedure Set_Handle_Hack(Object: in out Base_Object; Handle: Access_Type);

   not overriding function Get_Handle(Object: Base_Object) return Access_Type;

   not overriding function From_Handle(Handle: Access_Type) return Base_Object;

   not overriding function From_Non_Null_Handle(Handle: Access_Type) return Base_Object;

   not overriding function Default_Handle(Object: Base_Object) return Access_Type;

   not overriding procedure Finalize_Handle(Object: Base_Object; Handle: Access_Type) is null;

   not overriding function Adjust_Handle(Object: Base_Object; Handle: Access_Type) return Access_Type;

   not overriding function Is_Null (Object: Base_Object) return Boolean;

   generic
      type Base is new Base_Object with private;
   package With_Finalization is
      type Derived is new Base with null record;
      overriding procedure Finalize(Object: in out Derived)
                                    renames Do_Finalize;
      overriding procedure Adjust(Object: in out Derived)
                                  renames Do_Adjust;
      not overriding function Copy(Object: Base'Class) return Derived;
   end;

private

   type Base_Object is new Ada.Finalization.Controlled with
      record
         Handle: Access_Type;
      end record;

end RDF.Auxiliary.Handled_Record;
