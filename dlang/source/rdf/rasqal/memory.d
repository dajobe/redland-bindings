module rdf.rasqal.memory;

extern extern(C) {
   void rasqal_free_memory (char* ptr);
   char* rasqal_alloc_memory(size_t size);
   char* rasqal_calloc_memory (size_t nmemb, size_t size);
}

// Missing in C code, so I implement it in D
char *copy_c_string(const char* str) {
    import core.stdc.string;
    immutable size_t len = strlen(str) + 1;
    char* newStr = rasqal_alloc_memory(len);
    return strncpy(newStr, str, len);
}
