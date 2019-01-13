module rdf.redland.statement;

import std.stdio : File, FILE;
import rdf.auxiliary.handled_record;
static import rdf.raptor.statement;
import rdf.redland.world;
import rdf.redland.node;

struct StatementHandle;

enum StatementPartFlags {
   SubjectPart   = 1,
   PredicatePart = 2,
   ObjectPart    = 4,
   AllParts      = SubjectPart | PredicatePart | ObjectPart }

private extern extern(C) {
    void librdf_free_statement(StatementHandle* statement);
    StatementHandle* librdf_new_statement_from_statement(StatementHandle* statement);
    void librdf_statement_clear(StatementHandle* statement);
    NodeHandle* librdf_statement_get_subject(StatementHandle* statement);
    void librdf_statement_set_subject(StatementHandle* statement, NodeHandle* node);
    NodeHandle* librdf_statement_get_predicate(StatementHandle* statement);
    void librdf_statement_set_predicate(StatementHandle* statement, NodeHandle* node);
    NodeHandle* librdf_statement_get_object(StatementHandle* statement);
    void librdf_statement_set_object(StatementHandle* statement, NodeHandle* node);
    NodeHandle* librdf_new_node_from_node(NodeHandle* node);
    int librdf_statement_is_complete(StatementHandle* statement);
    void librdf_statement_print(StatementHandle* statement, FILE *fh);
    int librdf_statement_equals(StatementHandle* statement1, StatementHandle* statement2);
}

struct StatementWithoutFinalize {
    mixin WithoutFinalize!(StatementHandle,
                           StatementWithoutFinalize,
                           Statement,
                           librdf_new_statement_from_statement);
    bool opEquals(StatementWithoutFinalize other) {
        return librdf_statement_equals(handle, other.handle) != 0;
    }
    @property rdf.raptor.statement.Statement toRaptor() { // FIXME: also dup() in Ada
      return rdf.raptor.statement.StatementWithoutFinalize.fromHandle(
        cast(rdf.raptor.statement.StatementHandle*)handle).dup;
    }
    void clear() {
        librdf_statement_clear(handle);
    }
    @property NodeWithoutFinalize subject() {
        return NodeWithoutFinalize.fromHandle(librdf_statement_get_subject(handle));
    }
    @property NodeWithoutFinalize predicate() {
        return NodeWithoutFinalize.fromHandle(librdf_statement_get_predicate(handle));
    }
    @property NodeWithoutFinalize object() {
        return NodeWithoutFinalize.fromHandle(librdf_statement_get_object(handle));
    }
    @property void subject(NodeWithoutFinalize node) {
        librdf_statement_set_subject(handle, librdf_new_node_from_node(node.handle));
    }
    @property void predicate(NodeWithoutFinalize node) {
        librdf_statement_set_predicate(handle, librdf_new_node_from_node(node.handle));
    }
    @property void object(NodeWithoutFinalize node) {
        librdf_statement_set_object(handle, librdf_new_node_from_node(node.handle));
    }
    @property bool isComplete() {
        return librdf_statement_is_complete(handle) != 0;
    }
    void print(File file) {
        librdf_statement_print(handle, file.getFP);
    }
}

struct Statement {
    mixin WithFinalize!(StatementHandle,
                        StatementWithoutFinalize,
                        Statement,
                        librdf_free_statement);
    bool opEquals(Statement other) {
        return librdf_statement_equals(handle, other.handle) != 0;
    }

    static Statement fromRaptor(rdf.raptor.statement.StatementWithoutFinalize uri) { // FIXME: also dup() in Ada
        return StatementWithoutFinalize.fromHandle(cast(StatementHandle*)uri.handle).dup;
    }
}

// Stopped at Match

