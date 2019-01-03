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
    TermHandle* _subject, _predicate, _object, _graph;
}

private extern extern(C) {
    StatementHandle* raptor_statement_copy(StatementHandle* statement);
    void raptor_free_statement(StatementHandle* statement);
    int raptor_statement_equals(const StatementHandle* s1, const StatementHandle* s2);
    int raptor_statement_compare(const StatementHandle* s1, const StatementHandle* s2);
}

struct StatementWithoutFinalize {
    mixin WithoutFinalize!(StatementHandle,
                           StatementWithoutFinalize,
                           Statement,
                           raptor_statement_copy);
    mixin CompareHandles!(raptor_statement_equals, raptor_statement_compare);
    @property RaptorWorldWithoutFinalize world() {
        return RaptorWorldWithoutFinalize.fromNonnullHandle(handle._world);
    }
    @property TermWithoutFinalize subject() { return TermWithoutFinalize.fromHandle(handle._subject); }
    @property TermWithoutFinalize predicate() { return TermWithoutFinalize.fromHandle(handle._predicate); }
    @property TermWithoutFinalize object() { return TermWithoutFinalize.fromHandle(handle._object); }
    @property TermWithoutFinalize graph() { return TermWithoutFinalize.fromHandle(handle._graph); }
}

struct Statement {
    mixin WithFinalize!(StatementHandle,
                        StatementWithoutFinalize,
                        Statement,
                        raptor_free_statement);
    mixin CompareHandles!(raptor_statement_equals, raptor_statement_compare);
}

// TODO: Stopped at Print
