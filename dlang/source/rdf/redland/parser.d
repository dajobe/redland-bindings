module rdf.redland.parser;

import std.typecons;
import std.string;
import rdf.auxiliary.handled_record;
import rdf.auxiliary.nullable_string;
import rdf.raptor.syntax;
import rdf.redland.world;

struct ParserHandle;

private extern extern(C) {
    void librdf_free_parser(ParserHandle* parser);
    int librdf_parser_check_name(RedlandWorldHandle* world, const char *name);
    const(char*) librdf_parser_guess_name2(RedlandWorldHandle* world,
                                           const char *mime_type,
                                           const char *buffer,
                                           const char *identifier);
    const(SyntaxDescription*) librdf_parser_get_description(RedlandWorldHandle* world,
                                                            uint counter);
}

struct ParserWithoutFinalize {
    mixin WithoutFinalize!(ParserHandle,
                           ParserWithoutFinalize,
                           Parser);
}

struct Parser {
    mixin WithFinalize!(ParserHandle,
                        ParserWithoutFinalize,
                        Parser,
                        librdf_free_parser);
}

bool parserCheckName(RedlandWorldWithoutFinalize world, string name) {
    return librdf_parser_check_name(world.handle, name.toStringz) != 0;
}

// Order of arguments not the same as in C
string parserGuessName(RedlandWorldWithoutFinalize world,
                       string mimeType,
                       string identifier,
                       Nullable!string buffer = Nullable!string())
{
    const char* result = librdf_parser_guess_name2(world.handle,
                                                   mimeType.empty ? null : mimeType.ptr,
                                                   buffer.myToStringz,
                                                   identifier.empty ? null : identifier.ptr);
    return result ? result.fromStringz.idup : "";
}

ref const(SyntaxDescription) getParserDescription (RedlandWorldWithoutFinalize world, uint counter)
{
    return *librdf_parser_get_description(world.handle, counter);
}

struct ParsersEnumerate {
private:
    RedlandWorldWithoutFinalize _world;
    uint counter = 0;
public:
    this(RedlandWorldWithoutFinalize world) {
        _world = world;
    }
    @property uint position() { return counter; }
    @property bool empty() {
        return !librdf_parser_get_description(_world.handle, counter);
    }
    @property ref const(SyntaxDescription) front()
        in(!empty)
    {
        return getParserDescription(_world, counter);
    }
    void popFront()
        in(!empty)
    {
        ++counter;
    }
}


// TODO: Stopped at As_Stream

