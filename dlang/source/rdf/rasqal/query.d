module rdf.rasqal.query;

import rdf.auxiliary.handled_record;

// This API is partial, as I consider many C functions as internals.

struct QueryHandle;

private extern extern(C) {
    void rasqal_free_query(QueryHandle* query);
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
                        rasqal_free_query);
}

// TODO

