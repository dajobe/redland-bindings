with Interfaces.C; use Interfaces.C;

package body RDF.Redland.Iterator is

   procedure librdf_free_iterator (Handle: Iterator_Handle)
     with Import, Convention=>C;

   procedure Finalize_Handle (Object: Iterator_Type_Without_Finalize; Handle: Iterator_Handle) is
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

   function librdf_iterator_get_object (Iterator: Iterator_Handle) return chars_ptr
     with Import, Convention=>C;
   function librdf_iterator_get_context (Iterator: Iterator_Handle) return chars_ptr
     with Import, Convention=>C;
   function librdf_iterator_get_key (Iterator: Iterator_Handle) return chars_ptr
     with Import, Convention=>C;
   function librdf_iterator_get_value (Iterator: Iterator_Handle) return chars_ptr
     with Import, Convention=>C;

   function Get_Object_Internal (Iterator: Iterator_Type_Without_Finalize) return chars_ptr is
     (librdf_iterator_get_object(Get_Handle(Iterator)));
   function Get_Context_Internal (Iterator: Iterator_Type_Without_Finalize) return chars_ptr is
     (librdf_iterator_get_context(Get_Handle(Iterator)));
   function Get_Key_Internal (Iterator: Iterator_Type_Without_Finalize) return chars_ptr is
     (librdf_iterator_get_key(Get_Handle(Iterator)));
   function Get_Value_Internal (Iterator: Iterator_Type_Without_Finalize) return chars_ptr is
     (librdf_iterator_get_value(Get_Handle(Iterator)));

   function librdf_new_empty_iterator return Iterator_Handle
     with Import, Convention=>C;

   function Empty_Iterator return Iterator_Type is
   begin
      return From_Non_Null_Handle(librdf_new_empty_iterator);
   end;

end RDF.Redland.Iterator;
