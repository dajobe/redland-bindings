with System; use System;
with System.Storage_Pools;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

-- This are thin bindings
package RDF.Raptor.Memory is

   procedure raptor_free_memory (ptr: chars_ptr)
     with Import, Convention=>C;

   function raptor_alloc_memory (size: size_t) return chars_ptr
     with Import, Convention=>C;

   function raptor_calloc_memory (nmemb: size_t; size: size_t) return chars_ptr
     with Import, Convention=>C;

   -- Missing in C code, so I implement it in Ada
   function Copy_C_String (Str: chars_ptr) return chars_ptr;

   type Raptor_Storage_Pool is new System.Storage_Pools.Root_Storage_Pool with null record;

   overriding procedure Allocate(Pool : in out Raptor_Storage_Pool;
                                 Storage_Address : out Address;
                                 Size_In_Storage_Elements : in Storage_Count;
                                 Alignment : in Storage_Count);

   overriding procedure Deallocate(Pool : in out Raptor_Storage_Pool;
                                   Storage_Address : in Address;
                                   Size_In_Storage_Elements : in Storage_Count;
                                   Alignment : in Storage_Count);

   overriding function Storage_Size(Pool : Raptor_Storage_Pool)
                                    return Storage_Count;

   Raptor_Storage_Pool_Instance: Raptor_Storage_Pool;

end RDF.Raptor.Memory;
