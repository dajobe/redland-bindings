module rdf.raptor.statement;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.memory;
import rdf.raptor.world;
// import rdf.raptor.uri;

struct StatementHandle;

private extern extern(C) {
    StatementHandle* raptor_statement_copy(StatementHandle* statement);
    void raptor_free_statement(StatementHandle* statement);
}

struct StatementWithoutFinalize {
    mixin WithoutFinalize!(StatementHandle,
                           StatementWithoutFinalize,
                           Statement,
                           raptor_statement_copy);
}

struct Statement {
    mixin WithFinalize!(StatementHandle,
                        StatementWithoutFinalize,
                        Statement,
                        raptor_free_statement);
}
