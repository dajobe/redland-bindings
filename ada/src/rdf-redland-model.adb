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

end RDF.Redland.Model;
