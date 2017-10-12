with Interfaces.C; use Interfaces.C;

package body RDF.Redland.Iterator is

   procedure librdf_free_iterator (Handle: Iterator_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Iterator_Type; Handle: Iterator_Handle) is
   begin
      librdf_free_iterator(Handle);
   end;

   function librdf_iterator_end (Iterator: Iterator_Handle) return int
     with Import, Convention=>C;

   function Is_End (Iterator: Iterator_Type_Without_Finalize) return Boolean is
      (librdf_iterator_end(Get_Handle(Iterator)) /= 0);

   function librdf_iterator_next (Iterator: Iterator_Handle) return int
     with Import, Convention=>C;

   procedure Next (Iterator: in out Iterator_Type_Without_Finalize) is
      Result: constant int := librdf_iterator_next(Get_Handle(Iterator));
      pragma Unreferenced(Result);
   begin
      null;
   end;

end RDF.Redland.Iterator;
