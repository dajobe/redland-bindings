with Interfaces.C.Strings; use Interfaces.C.Strings;

package body RDF.Redland.Model is

   type String_P_Type is access all chars_ptr with Convention=>C;

   function librdf_model_enumerate (World: Redland_World_Handle;
                                    Counter: unsigned;
                                    Name, Label: String_P_Type)
                                    return int
     with Import, Convention=>C;

   function Enumerate_Models (World: Redland_World_Type_Without_Finalize'Class;
                              Counter: unsigned)
                              return Model_Info_Holders.Holder is
      Name, Label: aliased chars_ptr;
      Result: constant int :=
        librdf_model_enumerate(Get_Handle(World), Counter, Name'Unchecked_Access, Label'Unchecked_Access);
      use Model_Info_Holders;
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

   function Has_Element (Position: Enumerate_Models_Cursor) return Boolean is
   begin
      return librdf_model_enumerate(Position.World, Position.Counter, null, null) = 0;
   end;

   function First (Object: Enumerate_Models_Iterator) return Enumerate_Models_Cursor is
   begin
      return (World => Object.World, Counter => 0);
   end;

   function Next (Object: Enumerate_Models_Iterator; Position: Enumerate_Models_Cursor)
                  return Enumerate_Models_Cursor is
   begin
      return (World => Position.World, Counter => Position.Counter + 1);
   end;

   function librdf_new_model (World: Redland_World_Handle; Storage: Storage_Handle; Options: char_array)
                              return Model_Handle
     with Import, Convention=>C;

   function Create (World: Redland_World_Type_Without_Finalize'Class;
                    Storage: Storage_Type_Without_Finalize'Class;
                    Options: String)
                    return Model_Type is
      Handle: constant Model_Handle :=
        librdf_new_model(Get_Handle(World), Get_Handle(Storage), To_C(Options));
   begin
      return From_Non_Null_Handle(Handle);
   end;

   function librdf_new_model_from_model (Model: Model_Handle) return Model_Handle
     with Import, Convention=>C;

   function Copy (Model: Model_Type_Without_Finalize'Class) return Model_Type is
   begin
      return From_Non_Null_Handle(librdf_new_model_from_model(Get_Handle(Model)));
   end;

   procedure librdf_free_model (Handle: Model_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Model_Type; Handle: Model_Handle) is
   begin
      librdf_free_model(Handle);
   end;

end RDF.Redland.Model;
