module rdf.rasqal.memory;

import std.typecons;

extern extern(C) {
   void rasqal_free_memory (char* ptr);
   char* rasqal_alloc_memory(size_t size);
   char* rasqal_calloc_memory (size_t nmemb, size_t size);
}

// Missing in C code, so I implement it in D
char *rasqal_copy_c_string(const char* str) {
    import core.stdc.string;
    immutable size_t len = strlen(str) + 1;
    char* newStr = rasqal_alloc_memory(len);
    return strncpy(newStr, str, len);
}

// Missing in C code, so I implement it in D
char* rasqal_new_string(string str) {
    import core.stdc.string;
    char* newStr = rasqal_alloc_memory(str.length+1);
    newStr[str.length] = '\0';
    return strncpy(newStr, str.ptr, str.length);
}

char* rasqal_new_string(Nullable!string str) {
    return str.isNull ? null : rasqal_new_string(str.get);
}
