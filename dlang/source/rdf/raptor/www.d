module rdf.raptor.www;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.auxiliary.user;
import rdf.raptor.uri;

private extern extern(C) {
    void raptor_free_www(WWWHandle* www);
    alias raptor_www_write_bytes_handler = void function(WWWHandle* www,
                                                         void* userdata,
                                                         const void* ptr,
                                                         size_t size,
                                                         size_t nmemb);
    alias raptor_www_content_type_handler = void function(WWWHandle* www,
                                                          void* userdata,
                                                          const char *content_type);
    alias raptor_uri_filter_func = int function(void *user_data, URIHandle* uri);
    alias raptor_www_final_uri_handler = void function(WWWHandle* www,
                                                       void* userdata,
                                                       URIHandle* final_uri);
    void raptor_www_set_write_bytes_handler(WWWHandle *www,
                                            raptor_www_write_bytes_handler handler,
                                            void *user_data);
    void raptor_www_set_content_type_handler(WWWHandle *www,
                                             raptor_www_content_type_handler handler,
                                             void *user_data);
    void raptor_www_set_uri_filter(WWWHandle *www, raptor_uri_filter_func filter, void *user_data);
    void raptor_www_set_final_uri_handler(WWWHandle *www,
                                          raptor_www_final_uri_handler handler,
                                          void *user_data);
}

/// I deliberately expose that it is a pointer type,
/// to simplify libcurl and libxml interaction
struct Connection;

struct WWWHandle;

struct WWWWithoutFinalize {
    mixin WithoutFinalize!(WWWHandle,
                           WWWWithoutFinalize,
                           WWW);
}

struct WWW {
    mixin WithFinalize!(WWWHandle,
                        WWWWithoutFinalize,
                        WWW,
                        raptor_free_www);
}

class UserWWW : UserObject!WWW {
    void initializeAllCallbacks() {
        initializeWriteBytesHandler();
        initializeContentTypeHandler();
        initializeURIFilter();
        initializeFinalURIHandler();
    }
    void initializeWriteBytesHandler() {
        raptor_www_set_write_bytes_handler(record.handle,
                                           &Write_Bytes_Handler_Impl,
                                           context);

    }
    void initializeContentTypeHandler() {
        raptor_www_set_content_type_handler(record.handle,
                                            &Content_Type_Handler_Impl,
                                            context);
    }
    void initializeURIFilter() {
        raptor_www_set_uri_filter(record.handle, &URI_Filter_Impl, context);
    }
    void initializeFinalURIHandler() {
        raptor_www_set_final_uri_handler(record.handle, &Final_URI_Handler_Impl, context);
    }

    void Write_Bytes_Handler (string value) { }
    void Content_Type_Handler (string Content_Type) { }
    void Final_URI_Handler (URIWithoutFinalize URI) { }

    /// Return False to disallow loading an URI
    bool URI_Filter (URIWithoutFinalize URI) {
        return true;
    }

    private static extern(C) {
        void Write_Bytes_Handler_Impl (WWWHandle *www,
                                            void* user_data,
                                            const void* ptr,
                                            size_t Size,
                                            size_t Nmemb)
        {
            (cast(UserWWW*)user_data).Write_Bytes_Handler(// fromHandle(www)), // ignored
                                                          (cast(immutable char*)ptr)[0..Size*Nmemb]);
        }
        void Content_Type_Handler_Impl (WWWHandle *www, void* user_data, const char* Content_Type)
        {
            (cast(UserWWW*)user_data).Content_Type_Handler(// fromHandle(www)), // ignored
                                (cast(immutable char*)Content_Type).fromStringz); // FIXME: is immutable right here
        }
        int URI_Filter_Impl (void* user_data, URIHandle* URI)
        {
            immutable bool Result = (cast(UserWWW*)user_data).URI_Filter(URIWithoutFinalize.fromNonnullHandle(URI));
            return Result ? 0 : 1;
        }
        void Final_URI_Handler_Impl (WWWHandle *www, void* user_data, URIHandle* URI) {
            (cast(UserWWW*)user_data).Final_URI_Handler(// fromHandle(www)), // ignored
                              URIWithoutFinalize.fromNonnullHandle(URI));
        }
    }
}

// TODO: Stopped at Set_User_Agent
