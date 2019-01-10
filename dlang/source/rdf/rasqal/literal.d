module rdf.rasqal.literal;

import std.string;
import std.stdio : File, FILE;
import rdf.auxiliary.handled_record;
import rdf.raptor.uri;
import rdf.rasqal.memory;
import rdf.rasqal.world;

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
    const(char*) rasqal_literal_type_label(LiteralType type);
    LiteralHandle* rasqal_new_typed_literal(RasqalWorldHandle* world,
                                            LiteralType type,
                                            const char *string);
    LiteralHandle* rasqal_new_boolean_literal(RasqalWorldHandle* world, int value);
    LiteralHandle* rasqal_new_decimal_literal(RasqalWorldHandle* world, const char *string);
    LiteralHandle* rasqal_new_double_literal(RasqalWorldHandle* world, double d);
    LiteralHandle* rasqal_new_float_literal(RasqalWorldHandle* world, float f);
    LiteralHandle* rasqal_new_floating_literal(RasqalWorldHandle* world,
                                               LiteralType type,
                                               double d);
    LiteralHandle* rasqal_new_numeric_literal_from_long(RasqalWorldHandle* world,
                                                        LiteralType type,
                                                        long value);
    LiteralHandle* rasqal_new_simple_literal(RasqalWorldHandle* world,
                                             LiteralType type,
                                             const char *string);
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
    Literal newTypedLiteral(RasqalWorldWithoutFinalize world,
                            LiteralType typeOfLiteral,
                            string value)
    {
        LiteralHandle* handle =
            rasqal_new_typed_literal(world.handle, typeOfLiteral, value.toStringz);
        return fromNonnullHandle(handle);
    }
    Literal fromBoolean(RasqalWorldWithoutFinalize world, bool value) {
        LiteralHandle* handle =
            rasqal_new_boolean_literal(world.handle, cast(int)value);
        return fromNonnullHandle(handle);
    }
    // Not implemented
    // Literal fromDatetime(RasqalWorldWithoutFinalize world,  XSD_Datetime value)
    Literal fromDecimal(RasqalWorldWithoutFinalize world, string value) {
        LiteralHandle* handle =
            rasqal_new_decimal_literal(world.handle, value.toStringz);
        return fromNonnullHandle(handle);
    }
    // Not implemented
    // Literal fromDecimal(RasqalWorldWithoutFinalize world, XSD_Decimal value) {
    Literal fromDouble(RasqalWorldWithoutFinalize world, double value) {
        return fromNonnullHandle(rasqal_new_double_literal(world.handle, value));
    }
    Literal fromFloat(RasqalWorldWithoutFinalize world, float value) {
        return fromNonnullHandle(rasqal_new_float_literal(world.handle, value));
    }
    Literal fromFloating(RasqalWorldWithoutFinalize world, LiteralType type, double value)
        in(type == LiteralType.Literal_Float || type == LiteralType.Literal_Double)
    {
        return fromNonnullHandle(rasqal_new_floating_literal(world.handle, type, value));
    }
    /// Deliberately accept only long integers, don't implement "int value".
    Literal fromInteger (RasqalWorldWithoutFinalize world, long value) {
        LiteralHandle* handle =
            rasqal_new_numeric_literal_from_long(world.handle, LiteralType.Literal_Integer, value);
        return fromNonnullHandle(handle);
    }
    Literal newSimpleLiteral(RasqalWorldWithoutFinalize world, LiteralType type, string value)
        in(type == LiteralType.Literal_Blank || type == LiteralType.Literal_Qname)
    {
        char* value2 = rasqal_copy_c_string(value.toStringz); // freed by rasqal_new_simple_literal // TODO
        return fromNonnullHandle(rasqal_new_simple_literal(world.handle, type, value2));
    }
    // TODO: Stopped at New_String_Literal
}

string typeLabel(LiteralType kind) {
    return rasqal_literal_type_label(kind).fromStringz.idup;
}

