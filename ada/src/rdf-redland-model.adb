with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.C_String_Holders;

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
      use RDF.Auxiliary.C_String_Holders;
      use Model_Info_Holders;
   begin
      if Result /= 0 then
         return Empty_Holder;
      end if;
      return To_Holder((Name => New_Holder(Name), Label => New_Holder(Label)));
   end;

end RDF.Redland.Model;
