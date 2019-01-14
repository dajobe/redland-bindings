module rdf.redland.query;

import rdf.auxiliary.handled_record;
import rdf.raptor.syntax;
import rdf.redland.world;

struct QueryHandle;

private extern extern(C) {
    void librdf_free_query(QueryHandle* query);
    const(SyntaxDescription*) librdf_query_language_get_description(RedlandWorldHandle* world,
                                                                    uint counter);
}

struct QueryWithoutFinalize {
    mixin WithoutFinalize!(QueryHandle,
                           QueryWithoutFinalize,
                           Query);
}

struct Query {
    mixin WithFinalize!(QueryHandle,
                        QueryWithoutFinalize,
                        Query,
                        librdf_free_query);
}

ref const(SyntaxDescription) getQueryLanguageDescription(RedlandWorldWithoutFinalize world, uint counter) {
    return *librdf_query_language_get_description(world.handle, counter);
}

// TODO: Stopped at Query_Language_Description_Cursor

