module rdf.redland.statement;

import std.stdio : File, FILE;
import rdf.auxiliary.handled_record;
static import rdf.raptor.statement;
import rdf.raptor.iostream;
import rdf.redland.world;
import rdf.redland.node;

struct StatementHandle;

enum StatementPartFlags {
   subjectPart   = 1,
   predicatePart = 2,
   objectPart    = 4,
   allParts      = subjectPart | predicatePart | objectPart }

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
    int librdf_statement_match(StatementHandle* statement, StatementHandle* partial_statement);
    size_t librdf_statement_encode2(RedlandWorldHandle* world,
                                    StatementHandle* statement,
                                    char *buffer,
                                    size_t length);
    size_t librdf_statement_encode_parts2(RedlandWorldHandle* world,
                                          StatementHandle* statement,
                                          NodeHandle* context_node,
                                          char *buffer,
                                          size_t length,
                                          StatementPartFlags fields);
    int librdf_statement_write(StatementHandle* statement, IOStreamHandle* iostr);
    StatementHandle* librdf_new_statement(RedlandWorldHandle* world);
    StatementHandle* librdf_new_statement_from_nodes(RedlandWorldHandle* world,
                                                     NodeHandle* subject,
                                                     NodeHandle* predicate,
                                                     NodeHandle* object);
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
    bool matchedBy(StatementWithoutFinalize partial) {
        return librdf_statement_match(handle, partial.handle) != 0;
    }
    bool matches(StatementWithoutFinalize full) {
        return full.matchedBy(this);
    }
    string encode(RedlandWorldWithoutFinalize world) {
        size_t length = librdf_statement_encode2(world.handle, handle, null, 0);
        char[] buffer = new char[length];
        cast(void)librdf_statement_encode2(world.handle, handle, buffer.ptr, length);
        return buffer.idup; // TODO: Is duplication really needed?
    }
    string encodeParts(RedlandWorldWithoutFinalize world,
                       StatementWithoutFinalize statement,
                       NodeWithoutFinalize contextNode,
                       StatementPartFlags fields)
    {
        size_t length = librdf_statement_encode_parts2(world.handle,
                                                       handle,
                                                       contextNode.handle,
                                                       null,
                                                       0,
                                                       fields);
        char[] buffer = new char[length];
        cast(void)librdf_statement_encode_parts2(world.handle,
                                                 handle,
                                                 contextNode.handle,
                                                 buffer.ptr,
                                                 length,
                                                 fields);
        return buffer.idup; // TODO: Is duplication really needed?
    }
    // librdf_statement_decode2() not implemented (not so important and somehow hard to do)
    void Write(IOStreamWithoutFinalize stream) {
        if(librdf_statement_write(handle, stream.handle) != 0)
            throw new RDFException();
    }
    // librdf_new_statement_from_statement2() not bound.
    // (It is unclear how this would interact with D copying.)
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
    static Statement create(RedlandWorldWithoutFinalize world) {
      return fromNonnullHandle(librdf_new_statement(world.handle));
    }
    static Statement fromNodes(RedlandWorldWithoutFinalize world,
                               NodeWithoutFinalize subject,
                               NodeWithoutFinalize predicate,
                               NodeWithoutFinalize object)
    {
        StatementHandle* handle =
            librdf_new_statement_from_nodes(world.handle,
                                            subject.handle,
                                            predicate.handle,
                                            object.handle);
        return fromNonnullHandle(handle);
    }
    // librdf_statement_init() not bound because we don't support statistially declared objects. // TODO: Check
}

