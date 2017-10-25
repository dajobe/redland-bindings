with Interfaces.C.Strings; use Interfaces.C.Strings;
with RDF.Auxiliary.Limited_Handled_Record;

package RDF.Redland.Iterator is

   -- Usually you should use Base_With_Finalization types, such as defined in RDF.Redland.Node_Iterator

   -- TODO: Necessarily test that it works as expected

   package Iterator_Handled_Record is new RDF.Auxiliary.Limited_Handled_Record(RDF.Auxiliary.Dummy_Record, RDF.Auxiliary.Dummy_Record_Access);

   type Iterator_Type_Without_Finalize is new Iterator_Handled_Record.Base_Object with null record;

   subtype Iterator_Handle is Iterator_Handled_Record.Access_Type;

   overriding procedure Finalize_Handle (Object: Iterator_Type_Without_Finalize; Handle: Iterator_Handle);

   not overriding function Is_End (Iterator: Iterator_Type_Without_Finalize) return Boolean;

   not overriding procedure Next (Iterator: in out Iterator_Type_Without_Finalize);

   -- Internal functions
   not overriding function Get_Object_Internal  (Iterator: Iterator_Type_Without_Finalize) return chars_ptr;
   not overriding function Get_Context_Internal (Iterator: Iterator_Type_Without_Finalize) return chars_ptr;
   not overriding function Get_Key_Internal     (Iterator: Iterator_Type_Without_Finalize) return chars_ptr;
   not overriding function Get_Value_Internal   (Iterator: Iterator_Type_Without_Finalize) return chars_ptr;

   -- librdf_iterator_add_map() not implemented

   package Finalizer is new Iterator_Handled_Record.With_Finalization(Iterator_Type_Without_Finalize);

   type Iterator_Type is new Finalizer.Base_With_Finalization with null record;

   not overriding function Empty_Iterator return Iterator_Type;

   -- Implementing Ada2012 iterators does not seem possible, because we start
   -- iterators from a return value of a function, not from a "Forward_Iterator" object

end RDF.Redland.Iterator;
