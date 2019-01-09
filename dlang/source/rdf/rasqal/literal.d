module rdf.rasqal.literal;

import rdf.auxiliary.handled_record;

struct LiteralHandle;

private extern extern(C) {
    void rasqal_free_literal(LiteralHandle* l);
}

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

struct LiteralWithoutFinalize {
    mixin WithoutFinalize!(LiteralHandle,
                           LiteralWithoutFinalize,
                           Literal);
}

struct Literal {
    mixin WithFinalize!(LiteralHandle,
                        LiteralWithoutFinalize,
                        Literal,
                        rasqal_free_literal);
}

// TODO: Stopped at function "=" (Left, Right: Literal_Type_Without_Finalize)
