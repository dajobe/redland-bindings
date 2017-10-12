with RDF.Auxiliary.Limited_Handled_Record;

package RDF.Redland.Model is

   -- Use Limited_Handled_Record because despite a model can be copied,
   -- copying may be a very expensive operation. Use Copy function instead.
   -- See also https://gcc.gnu.org/bugzilla/show_bug.cgi?id=62042
   package Model_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Model_Type_Without_Finalize is new Model_Handled_Record.Base_Object with null record;

   subtype Model_Handle is Model_Handled_Record.Access_Type;

   -- Stopped at librdf_model_enumerate()

end RDF.Redland.Model;
