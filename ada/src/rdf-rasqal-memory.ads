with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

-- This are thin bindings
package RDF.Rasqal.Memory is

   procedure rasqal_free_memory (ptr: chars_ptr)
     with Import, Convention=>C;

   function rasqal_alloc_memory (size: size_t) return chars_ptr
     with Import, Convention=>C;

   function rasqal_calloc_memory (nmemb: size_t; size: size_t) return chars_ptr
     with Import, Convention=>C;

   -- Missing in C code, so I implement it in Ada
   function Copy_C_String (Str: chars_ptr) return chars_ptr;

end RDF.Rasqal.Memory;
