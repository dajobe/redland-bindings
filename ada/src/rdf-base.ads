with RDF.Auxilary.Handled_Record;

package RDF.Base is

   -- Internal
   type Dummy_Record is null record;

   -- Internal
   type Dummy_Record_Access is access Dummy_Record;

   package Simple_Handled_Record is new RDF.Auxilary.Handled_Record(Dummy_Record, Dummy_Record_Access);

   subtype Simple_Base_Object is Simple_Handled_Record.Base_Object;

end RDF.Base;
