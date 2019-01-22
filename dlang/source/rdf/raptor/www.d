module rdf.raptor.www;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.auxiliary.user;
import rdf.raptor.memory;
import rdf.raptor.world;
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
    // raptor_www_set_user_agent() & raptor_www_set_proxy() & raptor_www_set_http_accept()
    // don't return an error on out-of-memory: http://bugs.librdf.org/mantis/view.php?id=649
    void raptor_www_set_user_agent(WWWHandle *www, const char *user_agent);
    void raptor_www_set_proxy(WWWHandle *www, const char *proxy);
    void raptor_www_set_http_accept(WWWHandle *www, const char *value);
    int raptor_www_set_http_cache_control(WWWHandle *www, const char *cache_control);
    void raptor_www_set_connection_timeout(WWWHandle *www, int timeout);
    URIHandle* raptor_www_get_final_uri(WWWHandle* www);
    int raptor_www_fetch(WWWHandle* www, URIHandle* uri);
    alias raptor_data_malloc_handler = void* function(size_t size);
    int raptor_www_fetch_to_string(WWWHandle* www,
                                   URIHandle* uri,
                                   void **string_p,
                                   size_t *length_p,
                                   raptor_data_malloc_handler malloc_handler);
    void* raptor_www_get_connection(WWWHandle* www);
    int raptor_www_set_ssl_cert_options(WWWHandle* www,
                                        const char *cert_filename,
                                        const char *cert_type,
                                        const char *cert_passphrase);
    int raptor_www_set_ssl_verify_options(WWWHandle* www, int verify_peer, int verify_host);
    void raptor_www_abort(WWWHandle* www, const char *reason);
    WWWHandle* raptor_new_www(RaptorWorldHandle* world);
    WWWHandle* raptor_new_www_with_connection(RaptorWorldHandle* world, void *connection);
}

struct WWWHandle;

private immutable(char*) emptyToNull(string s) {
    if(s == "") return null;
    return s.toStringz;
}

struct WWWWithoutFinalize {
    mixin WithoutFinalize!(WWWHandle,
                           WWWWithoutFinalize,
                           WWW);
    /// Empty string means no User-Agent header (I make the behavior the same as --user-agent="" in Wget.
    void setUserAgent(string userAgent) {
        raptor_www_set_user_agent(handle, userAgent.emptyToNull);
    }
    void setProxy(string userAgent) {
        raptor_www_set_proxy(handle, userAgent.toStringz);
    }
    void setHTTPAccept(string value) {
        raptor_www_set_http_accept(handle, value.emptyToNull);
    }
    void setCacheControl(string value) {
        if(raptor_www_set_http_cache_control(handle, value.toStringz))
            throw new RDFException();
    }
    /// Remove Cache-Control: header altogether
    void unsetCacheControl() {
        if(raptor_www_set_http_cache_control(handle, null))
            throw new RDFException();
    }
    void setConnectionTimeout(uint seconds) {
        raptor_www_set_connection_timeout(handle, seconds);
    }
    URI getFinalURI() {
      // may return object with NULL handle
      return URI.fromHandle(raptor_www_get_final_uri(handle));
    }
    void fetch(URIWithoutFinalize uri) {
        if(raptor_www_fetch(handle, uri.handle) != 0)
            throw new RDFException();
    }
    string fetchToString(URIWithoutFinalize uri) {
        char* str;
        size_t len;
        if(raptor_www_fetch_to_string(handle, uri.handle, cast(void**)&str, &len, null) != 0)
            throw new RDFException();
        scope(exit) raptor_free_memory(str);
        return str[0..len].idup;
    }
    void* getConnection() {
        return raptor_www_get_connection(handle);
    }
    void setSSLCertOptions(string certFilename, string certType, string certPassphrase) {
        immutable int res = raptor_www_set_ssl_cert_options(handle,
                                                            certFilename.toStringz,
                                                            certType.toStringz,
                                                            certPassphrase.toStringz);
        if(res != 0) throw new RDFException();
    }
    void setSSLVerifyOptions(bool verifyPeer, bool verifyHost) {
        if(raptor_www_set_ssl_verify_options(handle, verifyPeer ? 1 : 0, verifyHost ? 1 : 0) != 0)
            throw new RDFException();
    }
    void abortOperation(string reason) {
        raptor_www_abort(handle, reason.toStringz);
    }
}

struct WWW {
    mixin WithFinalize!(WWWHandle,
                        WWWWithoutFinalize,
                        WWW,
                        raptor_free_www);
    static WWW create(RaptorWorldWithoutFinalize world) {
        return fromNonnullHandle(raptor_new_www(world.handle));
    }
    static WWW create(RaptorWorldWithoutFinalize world, void* connection) {
        return fromNonnullHandle(raptor_new_www_with_connection(world.handle, connection));
    }
}

class UserWWW : UserObject {
    WWW record;
    this(RaptorWorldWithoutFinalize world) {
        record = WWW.create(world);
    }
    this(RaptorWorldWithoutFinalize world, void* connection) {
        record = WWW.create(world, connection);
    }
    final void initializeAllCallbacks() {
        initializeWriteBytesHandler();
        initializeContentTypeHandler();
        initializeURIFilter();
        initializeFinalURIHandler();
    }
    final void initializeWriteBytesHandler() {
        raptor_www_set_write_bytes_handler(record.handle,
                                           &writeBytesHandlerImpl,
                                           context);

    }
    final void initializeContentTypeHandler() {
        raptor_www_set_content_type_handler(record.handle,
                                            &contentTypeHandlerImpl,
                                            context);
    }
    final void initializeURIFilter() {
        raptor_www_set_uri_filter(record.handle, &uriFilterImpl, context);
    }
    final void initializeFinalURIHandler() {
        raptor_www_set_final_uri_handler(record.handle, &finalURIHandlerImpl, context);
    }

    void writeBytesHandler(string value) { }
    void contentTypeHandler(string contentType) { }
    void finalURIHandler(URIWithoutFinalize URI) { }

    /// Return False to disallow loading an URI
    bool uriFilter(URIWithoutFinalize URI) {
        return true;
    }

    private static extern(C) {
        void writeBytesHandlerImpl(WWWHandle *www,
                                   void* userData,
                                   const void* ptr,
                                   size_t Size,
                                   size_t Nmemb)
        {
            (cast(UserWWW)userData).writeBytesHandler(// fromHandle(www)), // ignored
                                                       (cast(immutable char*)ptr)[0..Size*Nmemb]);
        }
        void contentTypeHandlerImpl(WWWHandle *www, void* userData, const char* contentType) {
            (cast(UserWWW)userData).contentTypeHandler(// fromHandle(www)), // ignored
                                                        (cast(immutable char*)contentType).fromStringz);
        }
        int uriFilterImpl(void* userData, URIHandle* URI) {
            return !(cast(UserWWW)userData).uriFilter(URIWithoutFinalize.fromNonnullHandle(URI));
        }
        void finalURIHandlerImpl(WWWHandle *www, void* userData, URIHandle* URI) {
            (cast(UserWWW)userData).finalURIHandler(// fromHandle(www)), // ignored
                                                     URIWithoutFinalize.fromNonnullHandle(URI));
        }
    }
}

