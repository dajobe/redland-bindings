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

end RDF.Redland.Storage;
