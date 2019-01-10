module rdf.redland.memory;

import std.typecons;

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

// Missing in C code, so I implement it in D
char* librdf_new_string(string str) {
    import core.stdc.string;
    char* newStr = librdf_alloc_memory(str.length+1);
    newStr[str.length] = '\0';
    return strncpy(newStr, str.ptr, str.length);
}

char* librdf_new_string(Nullable!string str) {
    return str.isNull ? null : librdf_new_string(str.get);
}

