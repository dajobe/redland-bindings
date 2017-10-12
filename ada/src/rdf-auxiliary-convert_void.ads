with System.Address_To_Access_Conversions;
with Interfaces.C.Strings; use Interfaces.C.Strings;

-- FIXME: This package should be used in place of Unchecked_Conversion (which may fail e.g. because of fat pointers)

-- C11, 6.2.5, 28 (draft N1548):
-- A pointer to void shall have the same representation and alignment requirements as a pointer to a character type.
-- So we can use chars_ptr to mean void pointer in C.

generic
    type Object(<>) is limited private;
package RDF.Auxiliary.Convert_Void is

   package Address_Conversions is new System.Address_To_Access_Conversions(Object);

   subtype Object_Pointer is Address_Conversions.Object_Pointer;

   function To_Access (Void_Pointer: chars_ptr) return Object_Pointer;

   function To_C_Pointer (Pointer: Object_Pointer) return chars_ptr;

end RDF.Auxiliary.Convert_Void;
