module rdf.rasqal.data_graph;

import std.typecons;
import std.string;
import std.stdio : File, FILE;
import rdf.auxiliary.handled_record;
import rdf.auxiliary.nullable_string;
import rdf.raptor.uri;
import rdf.raptor.iostream;
import rdf.rasqal.world;

enum DataGraphFlags { none, // unused
                      named,
                      background }

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
    DataGraphHandle* rasqal_new_data_graph_from_data_graph(DataGraphHandle* dg);
    int rasqal_data_graph_print(DataGraphHandle* dg, FILE *fh);
    DataGraphHandle* rasqal_new_data_graph_from_iostream(RasqalWorldHandle* world,
                                                         IOStreamHandle* iostr,
                                                         URIHandle* base_uri,
                                                         URIHandle* name_uri,
                                                         uint flags,
                                                         const char *formatType,
                                                         const char *formatName,
                                                         URIHandle* formatURI);
    DataGraphHandle* rasqal_new_data_graph_from_uri(RasqalWorldHandle* world,
                                                    URIHandle* uri,
                                                    URIHandle* name_uri,
                                                    uint flags,
                                                    const char *formatType,
                                                    const char *formatName,
                                                    URIHandle* formatURI);
}

struct DataGraphWithoutFinalize {
    mixin WithoutFinalize!(DataGraphHandle,
                           DataGraphWithoutFinalize,
                           DataGraph,
                           rasqal_new_data_graph_from_data_graph);
    @property RasqalWorldWithoutFinalize world() const {
        return RasqalWorldWithoutFinalize.fromHandle(handle._world);
    }
    @property URIWithoutFinalize uri() const {
        return URIWithoutFinalize.fromHandle(handle._uri);
    }
    @property URIWithoutFinalize nameURI() const {
        return URIWithoutFinalize.fromHandle(handle._nameURI);
    }
    @property DataGraphFlags flags() const {
        return cast(DataGraphFlags)handle._flags;
    }
    @property string formatType() const {
        return handle._formatType.fromStringz.idup;
    }
    @property string formatName() const {
        return handle._formatName.fromStringz.idup;
    }
    @property URIWithoutFinalize formatURI() const {
        return URIWithoutFinalize.fromHandle(handle._formatURI);
    }
    @property IOStreamWithoutFinalize iostream() {
        return IOStreamWithoutFinalize.fromHandle(handle._ioStr);
    }
    @property URIWithoutFinalize baseURI() const {
        return URIWithoutFinalize.fromHandle(handle._baseURI);
    }
    @property uint usageCount() const {
        return handle._usage;
    }
    void print(File file) const {
        if(rasqal_data_graph_print(handle, file.getFP) != 0)
            throw new RDFException();
    }
}

struct DataGraph {
    mixin WithFinalize!(DataGraphHandle,
                        DataGraphWithoutFinalize,
                        DataGraph,
                        rasqal_free_data_graph);
    static DataGraph fromIOStream(RasqalWorldWithoutFinalize world,
                                  IOStreamWithoutFinalize iostream,
                                  URIWithoutFinalize baseURI,
                                  URIWithoutFinalize nameURI = URIWithoutFinalize.fromHandle(null),
                                  DataGraphFlags flags = DataGraphFlags.background,
                                  Nullable!string formatType = Nullable!string(),
                                  Nullable!string formatName = Nullable!string(),
                                  URIWithoutFinalize formatURI = URIWithoutFinalize.fromHandle(null))
    {
      DataGraphHandle* handle =
        rasqal_new_data_graph_from_iostream(world.handle,
                                            iostream.handle,
                                            baseURI.handle,
                                            nameURI.handle,
                                            flags,
                                            formatType.myToStringz,
                                            formatName.myToStringz,
                                            formatURI.handle);
        return DataGraph.fromHandle(handle);
    }
    static DataGraph fromURI(RasqalWorldWithoutFinalize world,
                             URIWithoutFinalize uri,
                             URIWithoutFinalize nameURI,
                             DataGraphFlags flags,
                             Nullable!string formatType,
                             Nullable!string formatName,
                             URIWithoutFinalize formatURI)
    {
        DataGraphHandle* handle =
                rasqal_new_data_graph_from_uri(world.handle,
                                               uri.handle,
                                               nameURI.handle,
                                               flags,
                                               formatType.myToStringz,
                                               formatName.myToStringz,
                                               formatURI.handle);
        return DataGraph.fromHandle(handle);
    }
}

