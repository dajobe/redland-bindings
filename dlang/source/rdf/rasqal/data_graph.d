module rdf.rasqal.data_graph;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.uri;
import rdf.raptor.iostream;
import rdf.rasqal.world;

enum DataGraphFlags { None, // unused
                      Named,
                      Background }

struct DataGraphHandle {
private:
    RasqalWorldHandle* _world;
    URIHandle* _uri, _nameURI;
    uint _flags;
    char* _formatType, _formatName;
    URIHandle* _formatURI;
    IOStreamHandle* _ioStr;
    URIHandle* _baseURI;
    int _usage;
}

private extern extern(C) {
    void rasqal_free_data_graph(DataGraphHandle* dg);
}

struct DataGraphWithoutFinalize {
    mixin WithoutFinalize!(DataGraphHandle,
                           DataGraphWithoutFinalize,
                           DataGraph);
    @property RasqalWorldWithoutFinalize world() {
        return RasqalWorldWithoutFinalize.fromHandle(handle._world);
    }
    @property URIWithoutFinalize uri() {
        return URIWithoutFinalize.fromHandle(handle._uri);
    }
    @property URIWithoutFinalize nameURI() {
        return URIWithoutFinalize.fromHandle(handle._nameURI);
    }
    @property DataGraphFlags flags() {
        return cast(DataGraphFlags)handle._flags;
    }
    @property string formatType() {
        return handle._formatType.fromStringz.idup;
    }
    @property string formatName() {
        return handle._formatName.fromStringz.idup;
    }
    @property URIWithoutFinalize formatURI() {
        return URIWithoutFinalize.fromHandle(handle._formatURI);
    }
    @property IOStreamWithoutFinalize iostream() {
        return IOStreamWithoutFinalize.fromHandle(handle._ioStr);
    }
    @property URIWithoutFinalize baseURI() {
        return URIWithoutFinalize.fromHandle(handle._baseURI);
    }
    @property uint usageCount() {
        return handle._usage;
    }
    // TODO: Stopped at Adjust_Handle
}

struct DataGraph {
    mixin WithFinalize!(DataGraphHandle,
                        DataGraphWithoutFinalize,
                        DataGraph,
                        rasqal_free_data_graph);
}
