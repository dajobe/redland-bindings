module rdf.redland.memory;

extern extern(C) {
   void librdf_free_memory (char* ptr);
   char* librdf_alloc_memory(size_t size);
   char* librdf_calloc_memory (size_t nmemb, size_t size);
}

// Missing in C code, so I implement it in D
char *librdf_copy_c_string(char* str) {
    import core.stdc.string;
    immutable size_t len = strlen(str) + 1;
    char* newStr = librdf_alloc_memory(len);
    return strncpy(newStr, str, len);
}
