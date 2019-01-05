module rdf.raptor.iostream;

import rdf.auxiliary.handled_record;

struct IOStreamHandle;

private extern extern(C) {
    void raptor_free_iostream(IOStreamHandle* iostr);
    int raptor_iostream_hexadecimal_write(uint integer, int width, IOStreamHandle* iostr);
    int raptor_iostream_read_bytes(void *ptr, size_t size, size_t nmemb, IOStreamHandle* iostr);
    int raptor_iostream_read_eof(IOStreamHandle* iostr);
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
    size_t readBytes (char *ptr, size_t size, size_t nmemb) {
        immutable int result = raptor_iostream_read_bytes(ptr, size, nmemb, handle);
        if(result < 0) throw new IOStreamException();
        return result;
    }
    @property bool eof() {
        return raptor_iostream_read_eof(handle) != 0;
    }
}

struct IOStream {
    mixin WithFinalize!(IOStreamHandle,
                        IOStreamWithoutFinalize,
                        IOStream,
                        raptor_free_iostream);
}

// TODO: Stopped at function Tell
