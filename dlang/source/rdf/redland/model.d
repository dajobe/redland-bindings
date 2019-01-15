module rdf.redland.model;

import std.typecons;
import std.string;
import rdf.auxiliary.handled_record;
import rdf.redland.world;
import rdf.redland.node;
import rdf.redland.statement;
import rdf.redland.node_iterator;
import rdf.redland.stream;

struct ModelHandle;

private extern extern(C) {
    void librdf_free_model(ModelHandle* model);
    int librdf_model_enumerate(RedlandWorldHandle* world,
                               const int counter,
                               const char **name,
                               const char **label);
    int librdf_model_size(ModelHandle* model);
    int librdf_model_add(ModelHandle* model,
                         NodeHandle* subject,
                         NodeHandle* predicate,
                         NodeHandle* object);
    int librdf_model_add_statement(ModelHandle* model, StatementHandle* statement);
    int librdf_model_add_statements(ModelHandle* model, StreamHandle* statement_stream);
    int librdf_model_context_add_statement(ModelHandle* model,
                                           NodeHandle* context,
                                           StatementHandle* statement);
    int librdf_model_context_add_statements(ModelHandle* model,
                                            NodeHandle* context,
                                            StreamHandle* stream);
    int librdf_model_remove_statement(ModelHandle* model, StatementHandle* statement);
    int librdf_model_context_remove_statement(ModelHandle* model,
                                              NodeHandle* context,
                                              StatementHandle* statement);
    int librdf_model_context_remove_statements(ModelHandle* model, NodeHandle* context);
    int librdf_model_contains_statement(ModelHandle* model, StatementHandle *statement);
    int librdf_model_has_arc_in(ModelHandle* model, NodeHandle *node, NodeHandle* property);
    int librdf_model_has_arc_out(ModelHandle* model, NodeHandle *node, NodeHandle* property);
    StreamHandle* librdf_model_as_stream(ModelHandle* model);
    StreamHandle* librdf_model_find_statements(ModelHandle* model, StatementHandle* statement);
    NodeIteratorHandle* librdf_model_get_sources(ModelHandle* model,
                                                 NodeHandle* arc,
                                                 NodeHandle* target);
}

struct ModelInfo {
    string name, label;
}

struct ModelWithoutFinalize {
    mixin WithoutFinalize!(ModelHandle,
                           ModelWithoutFinalize,
                           Model);
    size_t sizeWithoutException() {
        return librdf_model_size(handle);
    }
    size_t size() {
        size_t res = sizeWithoutException();
        if(res < 0) throw new RDFException();
        return res;
    }
    void add(NodeWithoutFinalize subject,
             NodeWithoutFinalize predicate,
             NodeWithoutFinalize object)
    {
        if(librdf_model_add(handle, subject.handle, predicate.handle, object.handle) != 0)
            throw new RDFException();
    }
    void add(StatementWithoutFinalize statement,
             NodeWithoutFinalize context = NodeWithoutFinalize.fromHandle(null))
        in(statement.isComplete)
    {
        if(librdf_model_context_add_statement(handle, context.handle, statement.handle) != 0)
            throw new RDFException();
    }
    void add(StreamWithoutFinalize statements,
             NodeWithoutFinalize context = NodeWithoutFinalize.fromHandle(null))
    {
        if(librdf_model_context_add_statements(handle, context.handle, statements.handle) != 0)
            throw new RDFException();
    }
    void remove(StatementWithoutFinalize statement,
                NodeWithoutFinalize context = NodeWithoutFinalize.fromHandle(null))
        in(statement.isComplete)
    {
        if(librdf_model_context_remove_statement(handle, context.handle, statement.handle) != 0)
            throw new RDFException();
    }
    void remove(StreamWithoutFinalize statements,
                NodeWithoutFinalize context = NodeWithoutFinalize.fromHandle(null))
    {
        if(librdf_model_context_remove_statements(handle, context.handle) != 0)
            throw new RDFException();
    }
    void remove(StatementWithoutFinalize statement)
        in(statement.isComplete)
    {
        if(librdf_model_remove_statement(handle, statement.handle) != 0)
            throw new RDFException();
    }
    bool contains(StatementWithoutFinalize statement) {
        int result = librdf_model_contains_statement(handle, statement.handle);
        if(result > 0) throw new RDFException();
        return result != 0;
    }
    bool hasArcIn(NodeWithoutFinalize node, NodeWithoutFinalize property) {
      return librdf_model_has_arc_in(handle, node.handle, property.handle) != 0;
    }
    bool hasArcOut(NodeWithoutFinalize node, NodeWithoutFinalize property) {
      return librdf_model_has_arc_out(handle, node.handle, property.handle) != 0;
    }
    Stream asStream() {
        return Stream.fromNonnullHandle(librdf_model_as_stream(handle));
    }
    Stream find(StatementWithoutFinalize statement) {
        return Stream.fromNonnullHandle(librdf_model_find_statements(handle, statement.handle));
    }
    // LIBRDF_MODEL_FIND_OPTION_MATCH_SUBSTRING_LITERAL not implemented.
    // librdf_model_find_statements_with_options() not implemented as requires rdf_hash.
    NodeIterator getSources(NodeWithoutFinalize arc, NodeWithoutFinalize target) {
        NodeIteratorHandle* h = librdf_model_get_sources(handle, arc.handle, target.handle);
        return NodeIterator.fromNonnullHandle(h);
    }
}

struct Model {
    mixin WithFinalize!(ModelHandle,
                        ModelWithoutFinalize,
                        Model,
                        librdf_free_model);
}

Nullable!ModelInfo enumerateModels(RedlandWorldWithoutFinalize world, uint counter) {
    char* name, label;
    int Result = librdf_model_enumerate(world.handle, counter, &name, &label);
    return Nullable!ModelInfo(ModelInfo(name.fromStringz.idup, label.fromStringz.idup));
}

struct ModelsEnumerate {
private:
    RedlandWorldWithoutFinalize _world;
    uint counter = 0;
public:
    @property bool empty() {
        return librdf_model_enumerate(_world.handle, counter, null, null) != 0;
    }
    @property ModelInfo front()
        in(!empty)
    {
        return enumerateModels(_world, counter);
    }
    void popFront()
        in(!empty)
    {
        ++counter;
    }
}

// TODO: Stopped at Get_Arcs

