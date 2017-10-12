with RDF.Auxiliary.Limited_Handled_Record;

package RDF.Redland.Storage is

   -- Use Limited_Handled_Record because despite a storage can be copied,
   -- copying may be a very expensive operation. Use Copy function instead.
   -- See also https://gcc.gnu.org/bugzilla/show_bug.cgi?id=62042
   package Storage_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Storage_Type_Without_Finalize is new Storage_Handled_Record.Base_Object with null record;

   subtype Storage_Handle is Storage_Handled_Record.Access_Type;

   -- Stopped at librdf_storage_enumerate()

end RDF.Redland.Storage;
