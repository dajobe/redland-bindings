with Ada.Containers.Indefinite_Holders;
with Ada.Iterator_Interfaces;
with Interfaces.C; use Interfaces.C;
with RDF.Auxiliary;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Redland.Storage; use RDF.Redland.Storage;

package RDF.Redland.Model is

   -- Use Limited_Handled_Record because despite a model can be copied,
   -- copying may be a very expensive operation. Use Copy function instead.
   -- See also https://gcc.gnu.org/bugzilla/show_bug.cgi?id=62042
   package Model_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Model_Type_Without_Finalize is new Model_Handled_Record.Base_Object with null record;

   subtype Model_Handle is Model_Handled_Record.Access_Type;

   type Model_Info (Name_Length, Label_Length: Natural) is
      record
         Name : String(1..Name_Length );
         Label: String(1..Label_Length);
      end record;

   package Model_Info_Holders is new Ada.Containers.Indefinite_Holders(Model_Info);

   function Enumerate_Models (World: Redland_World_Type_Without_Finalize'Class;
                              Counter: unsigned)
                              return Model_Info_Holders.Holder;

   type Enumerate_Models_Cursor is private;

   function Has_Element (Position: Enumerate_Models_Cursor) return Boolean;

   package Enumerate_Models_Interfaces is
     new Ada.Iterator_Interfaces(Enumerate_Models_Cursor, Has_Element);

   type Enumerate_Models_Iterator is new Enumerate_Models_Interfaces.Forward_Iterator with private;

   overriding function First (Object: Enumerate_Models_Iterator) return Enumerate_Models_Cursor;

   overriding function Next (Object: Enumerate_Models_Iterator; Position: Enumerate_Models_Cursor)
                             return Enumerate_Models_Cursor;

   not overriding function Size_Without_Exception (Model: Model_Type_Without_Finalize)
                                                   return Integer;

   not overriding function Size (Model: Model_Type_Without_Finalize) return Natural;

   type Model_Type is new Model_Type_Without_Finalize with null record;

   overriding procedure Finalize_Handle (Object: Model_Type; Handle: Model_Handle);

   not overriding function Create (World: Redland_World_Type_Without_Finalize'Class;
                                   Storage: Storage_Type_Without_Finalize'Class;
                                   Options: String)
                                   return Model_Type;

   -- librdf_new_model_with_options() not implemented, because librdf_hash is not implemented

   not overriding function Copy (Model: Model_Type_Without_Finalize'Class) return Model_Type;

   -- Stopped at librdf_model_add()

private

   type Enumerate_Models_Cursor is
      record
         World: Redland_World_Handle;
         Counter: unsigned;
      end record;

   type Enumerate_Models_Iterator is new Enumerate_Models_Interfaces.Forward_Iterator with
      record
         World: Redland_World_Handle;
      end record;

end RDF.Redland.Model;
