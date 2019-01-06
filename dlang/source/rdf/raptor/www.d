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
    alias raptor_uri_filter_func = int function(void *userData, URIHandle* uri);
    alias raptor_www_final_uri_handler = void function(WWWHandle* www,
                                                       void* userdata,
                                                       URIHandle* final_uri);
    void raptor_www_set_write_bytes_handler(WWWHandle *www,
                                            raptor_www_write_bytes_handler handler,
                                            void *userData);
    void raptor_www_set_content_type_handler(WWWHandle *www,
                                             raptor_www_content_type_handler handler,
                                             void *userData);
    void raptor_www_set_uri_filter(WWWHandle *www, raptor_uri_filter_func filter, void *userData);
    void raptor_www_set_final_uri_handler(WWWHandle *www,
                                          raptor_www_final_uri_handler handler,
                                          void *userData);
}

/// I deliberately expose that it is a pointer type,
/// to simplify libcurl and libxml interaction
struct Connection;

struct WWWHandle;

struct WWWWithoutFinalize {
    mixin WithoutFinalize!(WWWHandle,
                           WWWWithoutFinalize,
                           WWW);
   // Empty string means no User-Agent header (I make the behavior the same as --user-agent="" in Wget.
   void Set_User_Agent (string userAgent) {
      // TODO
   }
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
                                           &writeBytesHandlerImpl,
                                           context);

    }
    void initializeContentTypeHandler() {
        raptor_www_set_content_type_handler(record.handle,
                                            &contentTypeHandlerImpl,
                                            context);
    }
    void initializeURIFilter() {
        raptor_www_set_uri_filter(record.handle, &uriFilterImpl, context);
    }
    void initializeFinalURIHandler() {
        raptor_www_set_final_uri_handler(record.handle, &finalURIHandlerImpl, context);
    }

    void writeBytesHandler (string value) { }
    void contentTypeHandler (string Content_Type) { }
    void finalURIHandler (URIWithoutFinalize URI) { }

    /// Return False to disallow loading an URI
    bool uriFilter (URIWithoutFinalize URI) {
        return true;
    }

    private static extern(C) {
        void writeBytesHandlerImpl(WWWHandle *www,
                                   void* userData,
                                   const void* ptr,
                                   size_t Size,
                                   size_t Nmemb)
        {
            (cast(UserWWW*)userData).writeBytesHandler(// fromHandle(www)), // ignored
                                                       (cast(immutable char*)ptr)[0..Size*Nmemb]);
        }
        void contentTypeHandlerImpl(WWWHandle *www, void* userData, const char* Content_Type) {
            (cast(UserWWW*)userData).contentTypeHandler(// fromHandle(www)), // ignored
                                                        (cast(immutable char*)Content_Type).fromStringz); // FIXME: is immutable right here?
        }
        int uriFilterImpl (void* userData, URIHandle* URI) {
            // FIXME: Why negation? (also in Ada)
            return !(cast(UserWWW*)userData).uriFilter(URIWithoutFinalize.fromNonnullHandle(URI));
        }
        void finalURIHandlerImpl (WWWHandle *www, void* userData, URIHandle* URI) {
            (cast(UserWWW*)userData).finalURIHandler(// fromHandle(www)), // ignored
                                                     URIWithoutFinalize.fromNonnullHandle(URI));
        }
    }
}

// TODO: Stopped at Set_User_Agent
