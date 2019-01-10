module rdf.raptor.memory;

extern extern(C) {
   void raptor_free_memory (char* ptr);
   char* raptor_alloc_memory(size_t size);
   char* raptor_calloc_memory (size_t nmemb, size_t size);
}

// Missing in C code, so I implement it in D
char *raptor_copy_c_string(char* str) {
    import core.stdc.string;
    immutable size_t len = strlen(str) + 1;
    char* newStr = raptor_alloc_memory(len);
    return strncpy(newStr, str, len);
}

// Missing in C code, so I implement it in D
char *raptor_new_string(string str) {
    import core.stdc.string;
    char* newStr = raptor_alloc_memory(str.length+1);
    newStr[str.length] = '\0';
    return strncpy(newStr, str.ptr, str.length);
}

