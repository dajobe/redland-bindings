with Ada.Containers.Indefinite_Holders;
with Interfaces.C; use Interfaces.C;
with RDF.Auxiliary.Limited_Handled_Record;
with RDF.Redland.World; use RDF.Redland.World;
with RDF.Auxiliary;

package RDF.Redland.Model is

   -- Use Limited_Handled_Record because despite a model can be copied,
   -- copying may be a very expensive operation. Use Copy function instead.
   -- See also https://gcc.gnu.org/bugzilla/show_bug.cgi?id=62042
   package Model_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Model_Type_Without_Finalize is new Model_Handled_Record.Base_Object with null record;

   subtype Model_Handle is Model_Handled_Record.Access_Type;

   type Model_Info is
      record
         Name, Label: RDF.Auxiliary.String_Holders.Holder;
      end record;

   package Model_Info_Holders is new Ada.Containers.Indefinite_Holders(Model_Info);

   function Enumerate_Models (World: Redland_World_Type_Without_Finalize'Class;
                              Counter: unsigned)
                              return Model_Info_Holders.Holder;

   -- Stopped at librdf_model_enumerate()

end RDF.Redland.Model;
