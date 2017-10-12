with Interfaces.C.Strings; use Interfaces.C.Strings;

package body RDF.Redland.Storage is

   type String_P_Type is access all chars_ptr with Convention=>C;

   function librdf_storage_enumerate (World: Redland_World_Handle;
                                      Counter: unsigned;
                                      Name, Label: String_P_Type)
                                      return int
     with Import, Convention=>C;

   function Enumerate_Storages (World: Redland_World_Type_Without_Finalize'Class;
                                Counter: unsigned)
                                return Storage_Info_Holders.Holder is
      Name, Label: aliased chars_ptr;
      Result: constant int :=
        librdf_storage_enumerate(Get_Handle(World), Counter, Name'Unchecked_Access, Label'Unchecked_Access);
      use Storage_Info_Holders;
   begin
      if Result /= 0 then
         return Empty_Holder;
      end if;
      declare
         Name2 : constant String := Value(Name );
         Label2: constant String := Value(Label);
      begin
         return To_Holder((Name_Length  => Name2 'Length,
                           Label_Length => Label2'Length,
                           Name => Name2, Label => Label2));
      end;
   end;

   function Has_Element (Position: Enumerate_Storages_Cursor) return Boolean is
   begin
      return librdf_storage_enumerate(Position.World, Position.Counter, null, null) = 0;
   end;

   function First (Object: Enumerate_Storages_Iterator) return Enumerate_Storages_Cursor is
   begin
      return (World => Object.World, Counter => 0);
   end;

   function Next (Object: Enumerate_Storages_Iterator; Position: Enumerate_Storages_Cursor)
                  return Enumerate_Storages_Cursor is
   begin
      return (World => Position.World, Counter => Position.Counter + 1);
   end;

   function librdf_new_storage (World: Redland_World_Handle; Factory_Name, Name, Options: char_array)
                                return Storage_Handle
     with Import, Convention=>C;

   function Create (World: Redland_World_Type_Without_Finalize'Class;
                    Factory_Name: String;
                    Name: String;
                    Options: String)
                    return Storage_Type is
      Handle: constant Storage_Handle :=
        librdf_new_storage(Get_Handle(World), To_C(Factory_Name), To_C(Name), To_C(Options));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_new_storage_from_storage (Storage: Storage_Handle) return Storage_Handle
     with Import, Convention=>C;

   function Copy (Storage: Storage_Type_Without_Finalize'Class) return Storage_Type is
   begin
      return From_Non_Null_Handle(librdf_new_storage_from_storage(Get_Handle(Storage)));
   end;

   procedure librdf_free_storage (Handle: Storage_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Storage_Type; Handle: Storage_Handle) is
   begin
      librdf_free_storage(Handle);
   end;

   function librdf_storage_sync (Storage: Storage_Handle) return int
     with Import, Convention=>C;

   procedure Sync (Storage: Storage_Type_Without_Finalize) is
   begin
      if librdf_storage_sync(Get_Handle(Storage)) /= 0 then
         raise RDF.Auxiliary.RDF_Exception;
      end if;
   end;

end RDF.Redland.Storage;
