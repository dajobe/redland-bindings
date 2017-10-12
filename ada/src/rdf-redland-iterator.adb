package body RDF.Redland.Iterator is

   procedure librdf_free_iterator (Handle: Iterator_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Iterator_Type; Handle: Iterator_Handle) is
   begin
      librdf_free_iterator(Handle);
   end;

end RDF.Redland.Iterator;
