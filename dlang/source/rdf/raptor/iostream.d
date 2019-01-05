module rdf.raptor.iostream;

import std.string;
import std.stdio;
import rdf.auxiliary.handled_record;
import rdf.auxiliary.user;
import rdf.raptor.world;
import rdf.raptor.uri;
import rdf.raptor.term;

struct IOStreamHandle;

private extern extern(C) {
    void raptor_free_iostream(IOStreamHandle* iostr);
    int raptor_iostream_hexadecimal_write(uint integer, int width, IOStreamHandle* iostr);
    int raptor_iostream_read_bytes(void *ptr, size_t size, size_t nmemb, IOStreamHandle* iostr);
    int raptor_iostream_read_eof(IOStreamHandle* iostr);
    ulong raptor_iostream_tell(IOStreamHandle* iostr);
    int raptor_iostream_counted_string_write(const void* string, size_t len, IOStreamHandle* iostr);
    int raptor_iostream_decimal_write(int integer, IOStreamHandle* iostr);
    int raptor_iostream_write_byte(const int byte_, IOStreamHandle* iostr);
    int raptor_iostream_write_bytes(const void *ptr, size_t size, size_t nmemb, IOStreamHandle* iostr);
    int raptor_iostream_write_end(IOStreamHandle* iostr);
    int raptor_bnodeid_ntriples_write(const char *bnodeid, size_t len, IOStreamHandle* iostr);
    int raptor_string_escaped_write(const char *string,
                                    size_t len,  const char delim,
                                    uint flags,
                                    IOStreamHandle* iostr);
    int raptor_uri_escaped_write(URIHandle* uri,
                                 URIHandle* base_uri,
                                 uint flags,
                                 IOStreamHandle* iostr);
    int raptor_term_escaped_write(const TermHandle* term, uint flags, IOStreamHandle* iostr);
    int raptor_string_ntriples_write(const char *string,
                                     size_t len,
                                     char delim,
                                     IOStreamHandle* iostr);
    IOStreamHandle* raptor_new_iostream_from_sink(RaptorWorldHandle* world);
    IOStreamHandle* raptor_new_iostream_from_filename(RaptorWorldHandle* world, const char *filename);
    IOStreamHandle* raptor_new_iostream_from_file_handle(RaptorWorldHandle* world, FILE *handle);
    IOStreamHandle* raptor_new_iostream_to_sink(RaptorWorldHandle* world);
    IOStreamHandle* raptor_new_iostream_to_filename(RaptorWorldHandle* world, const char *filename);
    IOStreamHandle* raptor_new_iostream_to_file_handle(RaptorWorldHandle* world, FILE *handle);
    IOStreamHandle* raptor_new_iostream_from_handler(RaptorWorldHandle* world,
                                                     void *user_data,
                                                     const DispatcherType* handler);
}

// TODO: Make this instead wrapper over D streams: https://stackoverflow.com/a/54029257/856090

class IOStreamException: Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
    this(string file = __FILE__, size_t line = __LINE__) {
        this("IOStream error", file, line);
    }
}

enum EscapedWriteBitflags {
    BITFLAG_BS_ESCAPES_BF      = 1,
    BITFLAG_BS_ESCAPES_TNRU    = 2,
    BITFLAG_UTF8               = 4,
    BITFLAG_SPARQL_URI_ESCAPES = 8,

    // N-Triples - favour writing \u, \U over UTF8
    NTRIPLES_LITERAL = BITFLAG_BS_ESCAPES_TNRU | BITFLAG_BS_ESCAPES_BF,
    NTRIPLES_URI     = BITFLAG_SPARQL_URI_ESCAPES,

    // SPARQL literal allows raw UTF8 for printable literals
    SPARQL_LITERAL = BITFLAG_UTF8,

    // SPARQL long literal no BS-escapes allowe
    SPARQL_LONG_LITERAL = BITFLAG_UTF8,

    // SPARQL uri have to escape certain characters
    SPARQL_URI = BITFLAG_UTF8 | BITFLAG_SPARQL_URI_ESCAPES,

    // Turtle (2013) escapes are like SPARQL
    TURTLE_URI          = SPARQL_URI,
    TURTLE_LITERAL      = SPARQL_LITERAL,
    TURTLE_LONG_LITERAL = SPARQL_LONG_LITERAL,

    //- JSON literals \b \f \t \r \n and \u \U
    JSON_LITERAL = BITFLAG_BS_ESCAPES_TNRU | BITFLAG_BS_ESCAPES_BF,
}

struct IOStreamWithoutFinalize {
    mixin WithoutFinalize!(IOStreamHandle,
                           IOStreamWithoutFinalize,
                           IOStream);
    void hexadecimalWrite(uint value, uint width) {
        if(raptor_iostream_hexadecimal_write(value, width, handle) < 0)
            throw new IOStreamException();
    }
    size_t readBytes(char *ptr, size_t size, size_t nmemb) {
        immutable int result = raptor_iostream_read_bytes(ptr, size, nmemb, handle);
        if(result < 0) throw new IOStreamException();
        return result;
    }
    @property bool eof() {
        return raptor_iostream_read_eof(handle) != 0;
    }
    ulong tell() {
        return raptor_iostream_tell(handle);
    }
    void write(string value) {
        if(raptor_iostream_counted_string_write (value.ptr, value.length, handle) != 0)
            throw new IOStreamException();
    }
    void write(char c) {
        if(raptor_iostream_write_byte(c, handle) != 0)
            throw new IOStreamException();
    }
    void decimalWrite(int value) {
        if(raptor_iostream_decimal_write(value, handle) < 0)
            throw new IOStreamException();
    }
    // FIXME: return size_t? (here and in Ada)
    int writeBytes(char *ptr, size_t size, size_t nmemb) {
        immutable int result = raptor_iostream_write_bytes (ptr, size, nmemb, handle);
        if(result < 0) throw new IOStreamException();
        return result;
    }
    void writeEnd() {
        if(raptor_iostream_write_end(handle) != 0)
            throw new IOStreamException();
    }
    void bnodeidNtriplesWrite(string bnode) {
        if(raptor_bnodeid_ntriples_write(bnode.ptr, bnode.length, handle) != 0)
            throw new IOStreamException();
    }
    void escapedWrite(string value, char delim, EscapedWriteBitflags flags) {
        if(raptor_string_escaped_write(value.ptr, value.length, delim, flags, handle) != 0)
            throw new IOStreamException();
    }
    void uriEscapedWrite(URIWithoutFinalize uri,
                         URIWithoutFinalize baseURI,
                         EscapedWriteBitflags flags)
    {
        if(raptor_uri_escaped_write(uri.handle, baseURI.handle, flags, handle) != 0)
            throw new IOStreamException();
    }
    void termEscapedWrite(TermWithoutFinalize term, EscapedWriteBitflags flags) {
        if(raptor_term_escaped_write(term.handle, flags, handle) != 0)
            throw new IOStreamException();
    }
    void ntriplesWrite(string value, char delim) {
        if(raptor_string_ntriples_write(value.ptr, value.length, delim, handle) != 0)
            throw new IOStreamException();
    }
}

struct IOStream {
    mixin WithFinalize!(IOStreamHandle,
                        IOStreamWithoutFinalize,
                        IOStream,
                        raptor_free_iostream);
    static IOStream fromSink(RaptorWorldWithoutFinalize world) {
        return fromNonnullHandle(raptor_new_iostream_from_sink(world.handle));
    }
    static IOStream fromFilename(RaptorWorldWithoutFinalize world, string filename) {
        return fromNonnullHandle(raptor_new_iostream_from_filename(world.handle, filename.toStringz));
    }
    static IOStream fromFileHandle(RaptorWorldWithoutFinalize world, File file) {
        return fromNonnullHandle(raptor_new_iostream_from_file_handle(world.handle, file.getFP));
    }
    static IOStream toSink(RaptorWorldWithoutFinalize world) {
        return fromNonnullHandle(raptor_new_iostream_to_sink(world.handle));
    }
    static IOStream toFilename(RaptorWorldWithoutFinalize world, string filename) {
        return fromNonnullHandle(raptor_new_iostream_to_filename(world.handle, filename.toStringz));
    }
    static IOStream toFileHandle(RaptorWorldWithoutFinalize world, File file) {
        return fromNonnullHandle(raptor_new_iostream_to_file_handle(world.handle, file.getFP));
    }
}

private extern(C) {
    alias raptor_iostream_init_func = int function(void *context);
    alias raptor_iostream_finish_func = void function(void *context);
    alias raptor_iostream_write_byte_func = int function(void *context, int byte_);
    alias raptor_iostream_write_bytes_func = int function(void *context,
                                                          const void *ptr,
                                                          size_t size,
                                                          size_t nmemb);
    alias raptor_iostream_write_end_func = int function(void *context);
    alias raptor_iostream_read_bytes_func = int function(void *context,
                                                         void *ptr,
                                                         size_t size,
                                                         size_t nmemb);
    alias raptor_iostream_read_eof_func = int function(void *context);

    int raptor_iostream_write_byte_impl(void* context, int byte_) {
        try {
            (cast(UserIOStream*)context).doWriteByte(cast(char)byte_);
            return 0;
        }
        catch(Exception) {
            return 1;
      }
    }

    int raptor_iostream_write_bytes_impl(void* context, const void* ptr, size_t size, size_t nmemb) {
        try {
            int Result = (cast(UserIOStream*)context).doWriteBytes(cast(char*)ptr, size, nmemb);
            return Result;
        }   
        catch(Exception) {
            return -1;
        }
    }

    int raptor_iostream_write_end_impl(void* context) {
        try {
            (cast(UserIOStream*)context).doWrite_End();
            return 0;
        }
        catch(Exception) {
            return 1;
        }
    }

    int raptor_iostream_read_bytes_impl(void* context, void* ptr, size_t size, size_t nmemb) {
        try {
            return cast(int)(cast(UserIOStream*)context).doReadBytes(cast(char*)ptr, size, nmemb);
        }
        catch(Exception) {
            return -1;
        }
    }

    int raptor_iostream_read_eof_impl(void* context) {
        return (cast(UserIOStream*)context).doReadEof;
    }
}

extern(C) struct DispatcherType {
    int version_ = 2;
    // V1 functions
    raptor_iostream_init_func init = null;
    raptor_iostream_finish_func finish = null;
    raptor_iostream_write_byte_func write_byte;
    raptor_iostream_write_bytes_func write_bytes;
    raptor_iostream_write_end_func write_end;
    // V2 functions
    raptor_iostream_read_bytes_func read_bytes;
    raptor_iostream_read_eof_func read_eof;
}

// TODO: Make it shared between threads
private immutable DispatcherType Dispatch =
    { version_: 2,
      init: null,
      finish: null,
      write_byte : &raptor_iostream_write_byte_impl,
      write_bytes: &raptor_iostream_write_bytes_impl,
      write_end  : &raptor_iostream_write_end_impl,
      read_bytes : &raptor_iostream_read_bytes_impl,
      read_eof   : &raptor_iostream_read_eof_impl };

class UserIOStream : UserObject!IOStream {
    this(RaptorWorld world) {
        IOStreamHandle* handle = raptor_new_iostream_from_handler(world.handle,
                                                                  cast(void*)this,
                                                                  &Dispatch);
        record = IOStream.fromNonnullHandle(handle);
    }
    abstract void doWriteByte(char byte_);
    abstract int doWriteBytes(char* data, size_t size, size_t count);
    abstract void doWrite_End();
    abstract size_t doReadBytes(char* data, size_t size, size_t count);
    abstract bool doReadEof();
}

// TODO: Stopped at function Stream_From_String
