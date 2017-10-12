with Ada.Containers.Indefinite_Holders;
with Interfaces.C; use Interfaces.C;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Redland.World; use RDF.Redland.World;
with Ada.Iterator_Interfaces;
with RDF.Auxiliary;

package RDF.Redland.Storage is

   -- Use Limited_Handled_Record because despite a storage can be copied,
   -- copying may be a very expensive operation. Use Copy function instead.
   -- See also https://gcc.gnu.org/bugzilla/show_bug.cgi?id=62042
   package Storage_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Storage_Type_Without_Finalize is new Storage_Handled_Record.Base_Object with null record;

   subtype Storage_Handle is Storage_Handled_Record.Access_Type;

   type Storage_Info (Name_Length, Label_Length: Natural) is
      record
         Name : String(1..Name_Length );
         Label: String(1..Label_Length);
      end record;

   package Storage_Info_Holders is new Ada.Containers.Indefinite_Holders(Storage_Info);

   function Enumerate_Storages (World: Redland_World_Type_Without_Finalize'Class;
                                Counter: unsigned)
                                return Storage_Info_Holders.Holder;

   type Enumerate_Storages_Cursor is private;

   function Has_Element (Position: Enumerate_Storages_Cursor) return Boolean;

   package Enumerate_Storages_Interfaces is
     new Ada.Iterator_Interfaces(Enumerate_Storages_Cursor, Has_Element);

   type Enumerate_Storages_Iterator is new Enumerate_Storages_Interfaces.Forward_Iterator with private;

   overriding function First (Object: Enumerate_Storages_Iterator) return Enumerate_Storages_Cursor;

   overriding function Next (Object: Enumerate_Storages_Iterator; Position: Enumerate_Storages_Cursor)
                             return Enumerate_Storages_Cursor;

   type Storage_Type is new Storage_Type_Without_Finalize with null record;

   not overriding function Create (World: Redland_World_Type_Without_Finalize'Class;
                                   Factory_Name: String;
                                   Name: String;
                                   Options: String)
                                   return Storage_Type;

   -- TODO: librdf_new_storage_with_options() not implemented, because
   -- we have not implemented librdf_hash

   not overriding function Copy (Storage: Storage_Type_Without_Finalize'Class) return Storage_Type;

   -- Stopped at librdf_free_storage()

private

   type Enumerate_Storages_Cursor is
      record
         World: Redland_World_Handle;
         Counter: unsigned;
      end record;

   type Enumerate_Storages_Iterator is new Enumerate_Storages_Interfaces.Forward_Iterator with
      record
         World: Redland_World_Handle;
      end record;

end RDF.Redland.Storage;
