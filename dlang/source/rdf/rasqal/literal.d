module rdf.rasqal.literal;

import rdf.auxiliary.handled_record;

struct LiteralHandle;

enum LiteralType { Literal_Unknown, // internal
                   Literal_Blank,
                   Literal_URI,
                   Literal_String,
                   Literal_Xsd_String,
                   Literal_Boolean,
                   Literal_Integer,
                   Literal_Float,
                   Literal_Double,
                   Literal_Decimal,
                   Literal_Datetime,
                   Literal_Udt,
                   Literal_Pattern,
                   Literal_Qname,
                   Literal_Variable,
                   Literal_Date }

enum CompareFlags { Compare_None     = 0,
                    Compare_Nocase   = 1,
                    Compare_XQuery   = 2,
                    Compare_RDF      = 4,
                    Compare_URI      = 8,
                    Compare_Sameterm = 16 }

private extern extern(C) {
    void rasqal_free_literal(LiteralHandle* l);
    int rasqal_literal_same_term(LiteralHandle* l1, LiteralHandle* l2);
    const(char*) rasqal_literal_as_counted_string(LiteralHandle* l,
                                                  size_t *len_p,
                                                  int flags,
                                                  int *error_p);
}

struct LiteralWithoutFinalize {
    mixin WithoutFinalize!(LiteralHandle,
                           LiteralWithoutFinalize,
                           Literal);
    bool opEquals(LiteralWithoutFinalize other) {
        return rasqal_literal_same_term(handle, other.handle) != 0;
    }
    string toString(CompareFlags flags) {
        int error = 0;
        size_t length;
        const char* item = rasqal_literal_as_counted_string(handle,
                                                            &length,
                                                            cast(int)flags,
                                                            &error);
        if(error != 0) throw new RDFException();
        return item[0..length].idup;
    }
}

struct Literal {
    mixin WithFinalize!(LiteralHandle,
                        LiteralWithoutFinalize,
                        Literal,
                        rasqal_free_literal);
    bool opEquals(LiteralWithoutFinalize other) {
        return rasqal_literal_same_term(handle, other.handle) != 0;
    }
}

// TODO: Stopped at Compare
