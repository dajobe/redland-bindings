with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

-- This are thin bindings
package RDF.Raptor.Memory is

   procedure raptor_free_memory (ptr: chars_ptr)
     with Import, Convention=>C, External_Name=>"raptor_free_memory";

   function raptor_alloc_memory (size: size_t) return chars_ptr
     with Import, Convention=>C, External_Name=>"raptor_alloc_memory";

   function raptor_calloc_memory (nmemb: size_t; size: size_t) return chars_ptr
     with Import, Convention=>C, External_Name=>"raptor_calloc_memory";

   -- Missing in C code, so I implement it in Ada
   function Copy_C_String (Str: chars_ptr) return chars_ptr;

end RDF.Raptor.Memory;
