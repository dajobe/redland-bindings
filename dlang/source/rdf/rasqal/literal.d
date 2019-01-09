module rdf.rasqal.literal;

import std.stdio : File, FILE;
import rdf.auxiliary.handled_record;
import rdf.raptor.uri;

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
    int rasqal_literal_compare(LiteralHandle* l1, LiteralHandle* l2, int flags, int *error_p);
    URIHandle* rasqal_literal_datatype(LiteralHandle* l);
    LiteralType rasqal_literal_get_rdf_term_type(LiteralHandle* l);
    int rasqal_literal_is_rdf_literal(LiteralHandle* l);
    int rasqal_literal_print(LiteralHandle* l, FILE *fh);
    void rasqal_literal_print_type(LiteralHandle* l, FILE *fh);
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
    int compare(LiteralWithoutFinalize other, CompareFlags flags) {
        int error = 0;
        return rasqal_literal_compare(handle, other.handle, int(flags), &error);
    }
    @property URIWithoutFinalize datatype() {
        return URIWithoutFinalize.fromHandle(rasqal_literal_datatype(handle));
    }
    // TODO:
    // Not supported as of Rasqal 0.9.32
    // @property Nullable!string language()
    @property LiteralType rdfTermType() {
        return rasqal_literal_get_rdf_term_type(handle);
    }
    // TODO:
    // Not supported as of Rasqal 0.9.32
    // @property literalType type()
    bool isRdfLiteral () {
        return rasqal_literal_is_rdf_literal(handle) != 0;
    }
    void print(File file) {
        if(rasqal_literal_print(handle, file.getFP) != 0)
            throw new RDFException();
    }
    void printType(File file) {
        rasqal_literal_print_type(handle, file.getFP);
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

// TODO: Stopped at Type_Label
