module rdf.redland.stream;

import rdf.auxiliary.handled_record;
import rdf.redland.world;
import rdf.redland.node;
import rdf.redland.statement;

// TODO: Necessarily test that it works as expected

struct StreamHandle;

private extern extern(C) {
    void librdf_free_stream(StreamHandle* stream);
    int librdf_stream_end(StreamHandle* stream);
    int librdf_stream_next(StreamHandle* stream);
    StatementHandle* librdf_stream_get_object(StreamHandle* stream);
    NodeHandle* librdf_stream_get_context2(StreamHandle* stream);
}

struct StreamWithoutFinalize {
    mixin WithoutFinalize!(StreamHandle,
                           StreamWithoutFinalize,
                           Stream);
    @property bool empty() {
        return librdf_stream_end(handle) != 0;
    }
    @property StreamWithoutFinalize front() { return this; }
    void popFront() {
        cast(void)librdf_stream_next(handle);
    }
    @property StatementWithoutFinalize object() {
        return StatementWithoutFinalize.fromHandle(librdf_stream_get_object(handle));
    }
    @property NodeWithoutFinalize context() {
        return NodeWithoutFinalize.fromHandle(librdf_stream_get_context2(handle));
    }
    // librdf_stream_add_map() not implemented
}

struct Stream {
    mixin WithFinalize!(StreamHandle,
                        StreamWithoutFinalize,
                        Stream,
                        librdf_free_stream);
}

// TODO: Stopped at Write

