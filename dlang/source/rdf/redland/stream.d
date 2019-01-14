module rdf.redland.stream;

import rdf.auxiliary.handled_record;
import rdf.raptor.iostream;
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
    int librdf_stream_write(StreamHandle* stream, IOStreamHandle* iostr);
    StreamHandle* librdf_new_empty_stream(RedlandWorldHandle* world);
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
    void write(IOStreamWithoutFinalize raptorStream) {
        if(librdf_stream_write(handle, raptorStream.handle) != 0)
            throw new RDFException();
    }
}

struct Stream {
    mixin WithFinalize!(StreamHandle,
                        StreamWithoutFinalize,
                        Stream,
                        librdf_free_stream);
    static Stream emptyStream(RedlandWorldWithoutFinalize world) {
        return fromNonnullHandle(librdf_new_empty_stream(world.handle));
    }
}

// TODO: Stopped at From_Node_Iterator

