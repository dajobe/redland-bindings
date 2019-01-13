module rdf.redland.statement;

import rdf.auxiliary.handled_record;
static import rdf.raptor.statement;
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
    @property rdf.raptor.statement.Statement toRaptor() { // FIXME: also dup() in Ada
      return rdf.raptor.statement.StatementWithoutFinalize.fromHandle(
        cast(rdf.raptor.statement.StatementHandle*)handle).dup;
    }
}

struct Statement {
    mixin WithFinalize!(StatementHandle,
                        StatementWithoutFinalize,
                        Statement,
                        librdf_free_statement);
    static Statement fromRaptor(rdf.raptor.statement.StatementWithoutFinalize uri) { // FIXME: also dup() in Ada
        return StatementWithoutFinalize.fromHandle(cast(StatementHandle*)uri.handle).dup;
    }
}

// Stopped at Statement_Part_Flags

