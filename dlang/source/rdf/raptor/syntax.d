module rdf.raptor.syntax;

import std.string;
import rdf.raptor.world;
import rdf.raptor.uri;

enum SyntaxBitflags { needBaseURI = 1 }

private extern extern(C) {
    const(SyntaxDescription)* raptor_world_get_parser_description(RaptorWorldHandle* world,
                                                                          uint counter);
    const(SyntaxDescription)* raptor_world_get_serializer_description(RaptorWorldHandle* world,
                                                                              uint counter);
    int raptor_world_get_parsers_count(RaptorWorldHandle* world);
    int raptor_world_get_serializers_count(RaptorWorldHandle* world);
    int raptor_world_is_parser_name(RaptorWorldHandle* world, const char *name);
    int raptor_world_is_serializer_name(RaptorWorldHandle* world, const char *name);
    immutable(char*) raptor_world_guess_parser_name(RaptorWorldHandle* world,
                                                    URIHandle* uri,
                                                    const char *mime_type,
                                                    const char *buffer,
                                                    size_t len,
                                                    const char *identifier);
}

struct MimeTypeQ {
private:
    char* _mimeType;
    size_t _mimeTypeLen;
    char _Q;
public:
    @property string mimeType() const { return _mimeType[0.._mimeTypeLen].idup; }
    @property byte Q() const { return _Q; }
}

struct SyntaxDescription {
private:
    char** _names;
    uint _namesCount;
    char* _label;
    MimeTypeQ* _mimeTypes;
    uint _mimeTypesCount;
    char** uriStrings;
    uint uriStringsCount;
    SyntaxBitflags _flags;
public:
    @disable this(this);
    string name(uint index) const
        in(index < namesCount)
    {
        return _names[index].fromStringz.idup;
    }
    @property uint namesCount() const { return _namesCount; }
    @property string label() const { return _label.fromStringz.idup; }
    ref const(MimeTypeQ) mimeTypeInfo(uint index) const
        in(index < _mimeTypesCount)
    {
        return _mimeTypes[index];
    }
    @property uint mimeTypesCount() const { return _mimeTypesCount; }
    string uri(uint index) const
        in(index < uriStringsCount)
    {
        return uriStrings[index].fromStringz.idup;
    }
    @property uint urisCount() const { return uriStringsCount; }
    @property SyntaxBitflags flags() const { return _flags; }
}

struct ParserDescriptionIterator {
private:
    RaptorWorldWithoutFinalize _world;
    uint _pos = 0;
public:
    this(RaptorWorldWithoutFinalize world) {
        this._world = world;
    }
    @property uint position() const { return _pos; }
    @property ref const(SyntaxDescription) front() const {
        return *raptor_world_get_parser_description(_world.handle, _pos);
    }
    @property bool empty() const {
        return !raptor_world_get_parser_description(_world.handle, _pos);
    }
    void popFront()
        in(!empty)
        do {
            ++_pos;
        }
}

struct SerializerDescriptionIterator {
private:
    RaptorWorldWithoutFinalize _world;
    uint _pos = 0;
public:
    this(RaptorWorldWithoutFinalize world) {
        this._world = world;
    }
    @property uint position() const { return _pos; }
    @property ref const(SyntaxDescription) front() const {
        return *raptor_world_get_serializer_description(_world.handle, _pos);
    }
    @property bool empty() const {
        return !raptor_world_get_serializer_description(_world.handle, _pos);
    }
    void popFront()
        in(!empty)
        do {
            ++_pos;
        }
}

bool isParserName(RaptorWorldWithoutFinalize world, string name) {
    return raptor_world_is_parser_name(world.handle, name.toStringz) != 0;
}

bool isSerializerName(RaptorWorldWithoutFinalize world, string name) {
    return raptor_world_is_serializer_name(world.handle, name.toStringz) != 0;
}

string guessParserName(RaptorWorldWithoutFinalize world,
                       URIWithoutFinalize uri,
                       string mimeType,
                       string buffer,
                       string identifier)
{
    immutable char* str = raptor_world_guess_parser_name(world.handle,
                                                         uri.handle,
                                                         mimeType.toStringz,
                                                         buffer.ptr,
                                                         buffer.length,
                                                         identifier.toStringz);
    // I don't use .idup because it is valid as long as the world exists
    return str.fromStringz;
}

unittest {
    RaptorWorld world = RaptorWorld.createAndOpen();
    for(auto i = ParserDescriptionIterator(world); !i.empty; i.popFront()) { }
    for(auto i = SerializerDescriptionIterator(world); !i.empty; i.popFront()) { }
}
