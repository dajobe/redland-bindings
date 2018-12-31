module rdf.raptor.memory;

extern extern(C) {
   void raptor_free_memory (char* ptr);
   char* raptor_alloc_memory(size_t size);
   char* raptor_calloc_memory (size_t nmemb, size_t size);
}

// Missing in C code, so I implement it in D
char *copy_c_string(char* str) {
    import core.stdc.string;
    immutable size_t len = strlen(str) + 1;
    char* newStr = raptor_alloc_memory(len);
    return strncpy(newStr, str, len);
}
