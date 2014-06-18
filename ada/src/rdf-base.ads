with Ada.Finalization;

package RDF.Base is

   -- Internal
   type Dummy_Record is private;

   -- Internal
   type Dummy_Record_Access is access Dummy_Record;

   -- It is logically abstract, but not exactly abstract in Ada sense.
   -- It can't be abstract because the function From_Handle returns this type.
   type Base_Object is new Ada.Finalization.Limited_Controlled with private;

   overriding procedure Initialize(Object: in out Base_Object);

   overriding procedure Finalize(Object: in out Base_Object);

   not overriding function Get_Handle(Object: Base_Object) return Dummy_Record_Access with Inline;

   not overriding function From_Handle(Handle: Dummy_Record_Access) return Base_Object with Inline;

   not overriding function Default_Handle(Object: Base_Object) return Dummy_Record_Access;

   not overriding procedure Finalize_Handle(Object: Base_Object; Handle: Dummy_Record_Access) is null;

private

   type Dummy_Record is null record;

   type Base_Object is new Ada.Finalization.Limited_Controlled with
      record
         Handle: Dummy_Record_Access;
      end record;

end RDF.Base;
