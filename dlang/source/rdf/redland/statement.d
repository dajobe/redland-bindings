module rdf.redland.statement;

import rdf.auxiliary.handled_record;
import rdf.redland.world;

struct StatementHandle;

private extern extern(C) {
    void librdf_free_statement(StatementHandle* statement);
    StatementHandle* librdf_new_statement_from_statement(StatementHandle* statement);
}

struct StatementWithoutFinalize {
    mixin WithoutFinalize!(StatementHandle,
                           StatementWithoutFinalize,
                           Statement,
                           librdf_new_statement_from_statement);
}

struct Statement {
    mixin WithFinalize!(StatementHandle,
                        StatementWithoutFinalize,
                        Statement,
                        librdf_free_statement);
}

// Stopped at To_Raptor

