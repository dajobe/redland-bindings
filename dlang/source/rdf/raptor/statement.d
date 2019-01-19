module rdf.raptor.statement;

import std.string;
import std.stdio;
import rdf.auxiliary.handled_record;
import rdf.raptor.memory;
import rdf.raptor.world;
import rdf.raptor.term;
import rdf.raptor.iostream;

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
    int raptor_statement_print(const StatementHandle* statement, FILE *stream);
    int raptor_statement_print_as_ntriples(const StatementHandle* statement, FILE *stream);
    int raptor_statement_ntriples_write(const StatementHandle* statement,
                                        IOStreamHandle *iostr,
                                        int write_graph_term);
    StatementHandle* raptor_new_statement(RaptorWorldHandle *world);
    StatementHandle* raptor_new_statement_from_nodes(RaptorWorldHandle* world,
                                                     TermHandle* subject,
                                                     TermHandle* predicate,
                                                     TermHandle* object,
                                                     TermHandle* graph);
    TermHandle* raptor_term_copy(TermHandle* term);
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
    void print(File file) {
        if(raptor_statement_print(handle, file.getFP))
            throw new IOStreamException();
    }
    void printAsNtriples(File file) {
        if(raptor_statement_print_as_ntriples(handle, file.getFP))
            throw new IOStreamException();
    }
    // raptor_statement_init(), raptor_statement_clear() are not boound, because they are probably internal
    void ntriplesWrite(IOStreamWithoutFinalize stream, bool writeGraphTerm) {
        if(raptor_statement_ntriples_write(handle, stream.handle, writeGraphTerm))
            throw new IOStreamException();
    }
}

struct Statement {
    mixin WithFinalize!(StatementHandle,
                        StatementWithoutFinalize,
                        Statement,
                        raptor_free_statement);
    mixin CompareHandles!(raptor_statement_equals, raptor_statement_compare);
    static Statement create(RaptorWorldWithoutFinalize world) {
        return Statement.fromNonnullHandle(raptor_new_statement(world.handle));
    }
    /// Makes copies of the terms (unlike the C library)
    static Statement create(RaptorWorldWithoutFinalize world,
                            TermWithoutFinalize subject,
                            TermWithoutFinalize predicate,
                            TermWithoutFinalize object,
                            TermWithoutFinalize graph = TermWithoutFinalize.fromHandle(null))
    {
        StatementHandle* handle =
            raptor_new_statement_from_nodes(world.handle,
                                            raptor_term_copy(subject.handle),
                                            raptor_term_copy(predicate.handle),
                                            raptor_term_copy(object.handle),
                                            raptor_term_copy(graph.handle));
        return Statement.fromNonnullHandle(handle);
    }
}

