with RDF.Auxiliary.Limited_Handled_Record;

package RDF.Redland.Iterator is

   package Iterator_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Iterator_Type_Without_Finalize is new Iterator_Handled_Record.Base_Object with null record;

   subtype Iterator_Handle is Iterator_Handled_Record.Access_Type;

   -- Stopped here

end RDF.Redland.Iterator;
