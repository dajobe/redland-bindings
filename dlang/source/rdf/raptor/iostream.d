module rdf.raptor.iostream;

import rdf.auxiliary.handled_record;

struct IOStreamHandle;

private extern extern(C) {
    void raptor_free_iostream(IOStreamHandle* iostr);
}

// TODO: Make this instead wrapper over D streams

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
}

struct IOStream {
    mixin WithFinalize!(IOStreamHandle,
                        IOStreamWithoutFinalize,
                        IOStream,
                        raptor_free_iostream);
}

// TODO: Stopped at procedure Hexadecimal_Write
