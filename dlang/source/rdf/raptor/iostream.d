module rdf.raptor.iostream;

import rdf.auxiliary.handled_record;
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
}

struct IOStream {
    mixin WithFinalize!(IOStreamHandle,
                        IOStreamWithoutFinalize,
                        IOStream,
                        raptor_free_iostream);
}

// TODO: Stopped at procedure Ntriples_Write
