module rdf.redland.query;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.syntax;
import rdf.redland.world;
import rdf.redland.uri;

struct QueryHandle;

private extern extern(C) {
    void librdf_free_query(QueryHandle* query);
    const(SyntaxDescription*) librdf_query_language_get_description(RedlandWorldHandle* world,
                                                                    uint counter);
    QueryHandle* librdf_new_query(RedlandWorldHandle* world,
                                  const char *name,
                                  URIHandle* uri,
                                  const char *query_string,
                                  URIHandle* base_uri);
    QueryHandle* librdf_new_query_from_query(QueryHandle* old_query);
}

struct QueryWithoutFinalize {
    mixin WithoutFinalize!(QueryHandle,
                           QueryWithoutFinalize,
                           Query,
                           librdf_new_query_from_query);
}

struct Query {
    mixin WithFinalize!(QueryHandle,
                        QueryWithoutFinalize,
                        Query,
                        librdf_free_query);
    static Query create(RedlandWorldWithoutFinalize world,
                        string name,
                        string queryString,
                        URIWithoutFinalize uri     = URIWithoutFinalize.fromHandle(null),
                        URIWithoutFinalize baseURI = URIWithoutFinalize.fromHandle(null))
    {
        QueryHandle* handle = librdf_new_query(world.handle,
                                               name.toStringz,
                                               uri.handle,
                                               queryString.toStringz,
                                               baseURI.handle);
        return fromNonnullHandle(handle);
    }
}

ref const(SyntaxDescription) getQueryLanguageDescription(const RedlandWorldWithoutFinalize world, uint counter) {
    return *librdf_query_language_get_description(world.handle, counter);
}


struct QueryLanguageDescriptionIterator {
private:
    RedlandWorldWithoutFinalize _world;
    uint _pos = 0;
public:
    this(RedlandWorldWithoutFinalize world) {
        _world = world;
    }
    @property uint position() const { return _pos; }
    @property ref const(SyntaxDescription) front() const {
        return getQueryLanguageDescription(_world, _pos);
    }
    @property bool empty() const {
        return !librdf_query_language_get_description(_world.handle, _pos);
    }
    void popFront()
        in { assert(!empty); }
        do {
            ++_pos;
        }
}

unittest {
    // see main.d instead
}

