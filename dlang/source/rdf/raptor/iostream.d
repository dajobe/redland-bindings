module rdf.raptor.iostream;

import rdf.auxiliary.handled_record;

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
}

struct IOStream {
    mixin WithFinalize!(IOStreamHandle,
                        IOStreamWithoutFinalize,
                        IOStream,
                        raptor_free_iostream);
}

// TODO: Stopped at function Escaped_Write_Bitflags
