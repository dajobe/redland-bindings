module rdf.rasqal.data_graph;

import rdf.raptor.uri;
import rdf.raptor.iostream;
import rdf.rasqal.world;

struct DataGraphHandle {
    RasqalWorldHandle* World;
    URIHandle* URI, Name_URI;
    uint Flags;
    char* Format_Type, Format_Name;
    URIHandle* Format_URI;
    IOStreamHandle* IOStr;
    URIHandle* Base_URI;
    int Usage;
}

