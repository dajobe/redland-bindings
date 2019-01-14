module rdf.redland.query;

import rdf.auxiliary.handled_record;

struct QueryHandle;

private extern extern(C) {
    void librdf_free_query(QueryHandle* query);
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

// TODO: Stopped at Get_Query_Language_Description

