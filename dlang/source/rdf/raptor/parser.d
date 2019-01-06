module rdf.raptor.parser;

// import std.string;
import rdf.auxiliary.handled_record;

struct ParserHandle;

private extern extern(C) {
    void raptor_free_parser(ParserHandle* parser);
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
                        raptor_free_parser);
}
