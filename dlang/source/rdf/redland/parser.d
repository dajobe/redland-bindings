module rdf.redland.parser;

import rdf.auxiliary.handled_record;

struct ParserHandle;

private extern extern(C) {
    void librdf_free_parser(ParserHandle* parser);
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

// TODO: Stopped at Parser_Check_Name

