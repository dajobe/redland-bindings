module rdf.redland.model;

import std.typecons;
import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.iostream;
import rdf.redland.world;
import rdf.redland.uri;
import rdf.redland.node;
import rdf.redland.statement;
import rdf.redland.node_iterator;
import rdf.redland.stream;
import rdf.redland.query;
import rdf.redland.query_results;
import rdf.redland.model;
import rdf.redland.storage;

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
    NodeIteratorHandle* librdf_model_get_arcs(ModelHandle* model,
                                              NodeHandle* source,
                                              NodeHandle* target);
    NodeIteratorHandle* librdf_model_get_targets(ModelHandle* model,
                                                 NodeHandle* source,
                                                 NodeHandle* arc);
    NodeHandle* librdf_model_get_source(ModelHandle* model, NodeHandle* arc, NodeHandle* target);
    NodeHandle* librdf_model_get_arc(ModelHandle* model, NodeHandle* source, NodeHandle* target);
    NodeHandle* librdf_model_get_target(ModelHandle* model, NodeHandle* source, NodeHandle* arc);
    NodeIteratorHandle* librdf_model_get_arcs_in(ModelHandle* model, NodeHandle* node);
    NodeIteratorHandle* librdf_model_get_arcs_out(ModelHandle* model, NodeHandle* node);
    int librdf_model_add_submodel(ModelHandle* model, ModelHandle* sub_model);
    int librdf_model_remove_submodel(ModelHandle* model, ModelHandle* sub_model);
    StreamHandle* librdf_model_context_as_stream(ModelHandle* model, NodeHandle* context);
    int librdf_model_contains_context(ModelHandle* model, NodeHandle* context);
    int librdf_model_supports_contexts(ModelHandle* model);
    QueryResultsHandle* librdf_model_query_execute(ModelHandle* model, QueryHandle* query);
    int librdf_model_sync(ModelHandle* model);
    int librdf_model_load(ModelHandle* model,
                          URIHandle* uri,
                          const char* name,
                          const char* mime_type,
                          URIHandle* type_uri);
    char* librdf_model_to_counted_string(ModelHandle* model,
                                         URIHandle* uri,
                                         const char *name,
                                         const char *mime_type,
                                         URIHandle* type_uri,
                                         size_t *string_length_p);
    StreamHandle* librdf_model_find_statements_in_context(ModelHandle* model,
                                                          StatementHandle* statement,
                                                          NodeHandle* context_node);
    NodeIteratorHandle* librdf_model_get_contexts(ModelHandle* model);
    NodeHandle* librdf_model_get_feature(ModelHandle* model, URIHandle* feature);
    int librdf_model_set_feature(ModelHandle* model, URIHandle* feature, NodeHandle* value);
    int librdf_model_add_string_literal_statement(ModelHandle* model,
                                                  NodeHandle* subject,
                                                  NodeHandle* predicate,
                                                  const char *literal,
                                                  const char *xml_language,
                                                  int is_wf_xml);
    int librdf_model_add_typed_literal_statement(ModelHandle* model,
                                                 NodeHandle* subject,
                                                 NodeHandle* predicate,
                                                 const char* literal,
                                                 const char* xml_language,
                                                 URIHandle* datatype_uri);
    int librdf_model_write(ModelHandle* model, IOStreamHandle* iostr);
    ModelHandle* librdf_new_model_from_model(ModelHandle* model);
    ModelHandle* librdf_new_model(RedlandWorldHandle* world,
                                  StorageHandle* storage,
                                  const char *options_string);
}

string featureContexts = "http://feature.librdf.org/model-contexts";

struct ModelInfo {
    string name, label;
}

struct ModelWithoutFinalize {
    mixin WithoutFinalize!(ModelHandle,
                           ModelWithoutFinalize,
                           Model,
                           librdf_new_model_from_model);
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
    NodeIterator getArcs(NodeWithoutFinalize source, NodeWithoutFinalize target) {
        NodeIteratorHandle* h = librdf_model_get_arcs(handle, source.handle, target.handle);
        return NodeIterator.fromNonnullHandle(h);
    }
    NodeIterator getTargets(NodeWithoutFinalize source, NodeWithoutFinalize arc) {
        NodeIteratorHandle* h = librdf_model_get_targets(handle, source.handle, arc.handle);
        return NodeIterator.fromNonnullHandle(h);
    }
    Node getSource(NodeWithoutFinalize arc, NodeWithoutFinalize target) {
        NodeHandle* h = librdf_model_get_source(handle, arc.handle, target.handle);
        return Node.fromNonnullHandle(h);
    }
    Node getArc(NodeWithoutFinalize source, NodeWithoutFinalize target) {
        NodeHandle* h = librdf_model_get_arc(handle, source.handle, target.handle);
        return Node.fromNonnullHandle(h);
    }
    Node getTarget(NodeWithoutFinalize source, NodeWithoutFinalize arc) {
        NodeHandle* h = librdf_model_get_target(handle, source.handle, arc.handle);
        return Node.fromNonnullHandle(h);
    }
    NodeIterator getArcsIn(NodeWithoutFinalize node) {
        NodeIteratorHandle* h = librdf_model_get_arcs_in(handle, node.handle);
        return NodeIterator.fromNonnullHandle(h);
    }
    NodeIterator getArcOut(NodeWithoutFinalize node) {
        NodeIteratorHandle* h = librdf_model_get_arcs_out(handle, node.handle);
        return NodeIterator.fromNonnullHandle(h);
    }
    void addSubmodel(ModelWithoutFinalize submodel) {
        if(librdf_model_add_submodel(handle, submodel.handle) != 0)
            throw new RDFException();
    }
    void removeSubmodel(ModelWithoutFinalize submodel) {
        if(librdf_model_remove_submodel(handle, submodel.handle) != 0)
            throw new RDFException();
    }
    Stream contextAsStream(NodeWithoutFinalize context) {
        return Stream.fromNonnullHandle(librdf_model_context_as_stream(handle, context.handle));
    }
    bool containsContext(NodeWithoutFinalize context) {
        return librdf_model_contains_context(handle, context.handle) != 0;
    }
    @property bool supportsContext() {
        return librdf_model_supports_contexts(handle) != 0;
    }
    QueryResults queryExecute(QueryWithoutFinalize query) { // TODO: Remove Without_Finalize in Ada
        return QueryResults.fromNonnullHandle(librdf_model_query_execute(handle, query.handle));
    }
    void sync() {
        if(librdf_model_sync(handle) != 0)
            throw new RDFException();
    }
    //@property StorageWithoutFinalize storage() // TODO
    void load(URIWithoutFinalize uri,
              string name = "",
              string mimeType = "",
              URIWithoutFinalize typeURI = URIWithoutFinalize.fromHandle(null))
    {
        int res = librdf_model_load(handle,
                                    uri.handle,
                                    name == "" ? null : name.ptr,
                                    mimeType == "" ? null : mimeType.ptr,
                                    typeURI.handle);
        if(res != 0) throw new RDFException();
    }
    string toString(URIWithoutFinalize uri,
                    string name = "",
                    string mimeType = "",
                    URIWithoutFinalize typeURI = URIWithoutFinalize.fromHandle(null))
    {
        size_t length;
        char* result = librdf_model_to_counted_string(handle,
                                                      uri.handle,
                                                      name.empty ? null : name.ptr,
                                                      mimeType.empty ? null : mimeType.ptr,
                                                      typeURI.handle,
                                                      &length);
        return result[0..length].idup;
    }
    Stream findInContext(StatementWithoutFinalize statement, NodeWithoutFinalize context) {
        StreamHandle* h =
            librdf_model_find_statements_in_context(handle, statement.handle, context.handle);
        return Stream.fromNonnullHandle(h);
    }
    NodeIterator getContexts() {
        return NodeIterator.fromNonnullHandle(librdf_model_get_contexts(handle));
    }
    Node getFeature(URIWithoutFinalize feature) {
        return Node.fromHandle(librdf_model_get_feature(handle, feature.handle));
    }
    void setFeature(URIWithoutFinalize feature, NodeWithoutFinalize value) {
        if(librdf_model_set_feature(handle, feature.handle, value.handle) != 0)
            throw new RDFException();
    }
    void addStringLiteralStatement(NodeWithoutFinalize subject,
                                   NodeWithoutFinalize predicate,
                                   string literal,
                                   string language,
                                   bool isXML = false)
    {
        int res = librdf_model_add_string_literal_statement(handle,
                                                            subject.handle,
                                                            predicate.handle,
                                                            literal.toStringz,
                                                            language.toStringz,
                                                            isXML);
        if(res != 0) throw new RDFException();
    }
    void addTypedLiteralStatement(NodeWithoutFinalize subject,
                                  NodeWithoutFinalize predicate,
                                  string literal,
                                  string language,
                                  URIWithoutFinalize datatype)
    {
        int res = librdf_model_add_typed_literal_statement(handle,
                                                           subject.handle,
                                                           predicate.handle,
                                                           literal.toStringz,
                                                           language.toStringz,
                                                           datatype.handle);
        if(res != 0) throw new RDFException();
    }
    void write(IOStreamWithoutFinalize stream) {
        if(librdf_model_write(handle, stream.handle) != 0)
            throw new RDFException();
    }
}

struct Model {
    mixin WithFinalize!(ModelHandle,
                        ModelWithoutFinalize,
                        Model,
                        librdf_free_model);
    static Model create(RedlandWorldWithoutFinalize world,
                        StorageWithoutFinalize storage,
                        string options = "")
    {
        ModelHandle* h = librdf_new_model(world.handle, storage.handle, options.toStringz);
        return Model.fromNonnullHandle(h);
    }
    // librdf_new_model_with_options() not implemented, because librdf_hash is not implemented
}

Nullable!ModelInfo enumerateModels(RedlandWorldWithoutFinalize world, uint counter) {
    char* name, label;
    int result = librdf_model_enumerate(world.handle, counter, &name, &label);
    if(result != 0) return Nullable!ModelInfo();
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

// TODO: Stopped at Create

