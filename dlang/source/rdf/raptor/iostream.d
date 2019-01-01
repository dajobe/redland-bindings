module rdf.raptor.iostream;

import rdf.auxiliary.handled_record;

private extern extern(C) {
    void raptor_free_iostream(Dummy* iostr);
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
    mixin WithoutFinalize!(IOStreamWithoutFinalize,
                           IOStream);
}

struct IOStream {
    mixin WithFinalize!(IOStreamWithoutFinalize,
                        IOStream,
                        raptor_free_iostream);
}

// TODO: Stopped at procedure Hexadecimal_Write
