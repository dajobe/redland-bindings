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

   overriding procedure Finalize_Handle (Object: Storage_Type_Without_Finalize; Handle: Storage_Handle);

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

   not overriding procedure Sync (Storage: Storage_Type_Without_Finalize);

   -- Will implement after http://bugs.librdf.org/mantis/view.php?id=636 bug fix
--     not overriding function Get_Feature (Storage: Storage_Type_Without_Finalize;
--                                          Feature: URI_Type_Without_Finalize'Class)
--                                          return Node_Type_Without_Finalize;
--     not overriding procedure Set_Feature (Storage: Storage_Type_Without_Finalize;
--                                           Feature: URI_Type_Without_Finalize'Class;
--                                           Value: Node_Type_Without_Finalize'Class);

   not overriding function Get_World (Storage: Storage_Type_Without_Finalize)
                                      return Redland_World_Type_Without_Finalize;

   package Handlers is new Storage_Handled_Record.Common_Handlers(Storage_Type_Without_Finalize);

   type Storage_Type is new Handlers.Base_With_Finalization with null record;

   type Storage_Type_User is new Handlers.User_Type with null record;

   not overriding function Create (World: Redland_World_Type_Without_Finalize'Class;
                                   Factory_Name: String;
                                   Name: String;
                                   Options: String := "")
                                   return Storage_Type;

   not overriding function Copy (Storage: Storage_Type_Without_Finalize'Class) return Storage_Type;

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
