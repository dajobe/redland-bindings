module rdf.raptor.statement;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.memory;
import rdf.raptor.world;
import rdf.raptor.term;

extern(C)
struct StatementHandle {
private:
    RaptorWorldHandle* _world;
    int _usage;
    TermHandle _subject, _predicate, _object, _graph;
}

private extern extern(C) {
    StatementHandle* raptor_statement_copy(StatementHandle* statement);
    void raptor_free_statement(StatementHandle* statement);
}

struct StatementWithoutFinalize {
    mixin WithoutFinalize!(StatementHandle,
                           StatementWithoutFinalize,
                           Statement,
                           raptor_statement_copy);
    @property RaptorWorldWithoutFinalize world() {
        return RaptorWorldWithoutFinalize.fromNonnullHandle(handle._world);
    }
}

struct Statement {
    mixin WithFinalize!(StatementHandle,
                        StatementWithoutFinalize,
                        Statement,
                        raptor_free_statement);
}

// TODO: Stopped at Get_Subject
