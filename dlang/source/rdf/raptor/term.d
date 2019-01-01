module rdf.raptor.term;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.memory;
import rdf.raptor.world;
import rdf.raptor.uri;

private extern extern(C) {
    void raptor_free_term(TermHandle* term);
    TermHandle* raptor_term_copy(TermHandle* term);
    int raptor_term_compare(const TermHandle* t1, const TermHandle* t2);
    int raptor_term_equals(const TermHandle* t1, const TermHandle* t2);
}

extern(C)
enum TermKind { unknown = 0,
                URI     = 1,
                literal = 2,
                // unused type 3
                blank   = 4 }

extern(C)
struct TermLiteralValue {
private:
    char* str;
    uint len;
    URIHandle* datatype;
    char* language;
    uint languageLen;
}

extern(C)
struct TermBlankValue {
private:
    char* str;
    uint len;
}

extern(C)
union TermValue {
    URIHandle* uri;
    TermLiteralValue literal;
    TermBlankValue blank;
}

extern(C)
struct TermHandle {
private:
    RaptorWorldHandle* world;
    int usage; // intentionally not accessible from our D bindings
    TermKind kind;
    TermValue value;
}

struct TermWithoutFinalize {
    mixin WithoutFinalize!(TermHandle,
                           TermWithoutFinalize,
                           Term,
                           raptor_term_copy);
    mixin CompareHandles!(raptor_term_equals, raptor_term_compare);
    @property RaptorWorld world() {
        return RaptorWorld.fromHandle(handle.world);
    }
}

struct Term {
    mixin WithFinalize!(TermHandle,
                        TermWithoutFinalize,
                        Term,
                        raptor_free_term);
    mixin CompareHandles!(raptor_term_equals, raptor_term_compare);
}

// TODO: Stopped at Get_Kind
