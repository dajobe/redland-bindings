module rdf.raptor.iostream;

import rdf.auxiliary.handled_record;

struct IOStreamHandle;

private extern extern(C) {
    void raptor_free_iostream(IOStreamHandle* iostr);
    int raptor_iostream_hexadecimal_write(uint integer, int width, IOStreamHandle* iostr);
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
}

struct IOStream {
    mixin WithFinalize!(IOStreamHandle,
                        IOStreamWithoutFinalize,
                        IOStream,
                        raptor_free_iostream);
}

// TODO: Stopped at procedure Read_Bytes
