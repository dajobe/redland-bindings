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
    char* _str;
    uint _len;
    URIHandle* _datatype;
    char* _language;
    uint _languageLen;
public:
    @property string value() {
        return _str[0.._len].idup; // with idup it is more reliable
    }
    @property URIWithoutFinalize datatype() {
        return URIWithoutFinalize.fromHandle(_datatype);
    }
}

extern(C)
struct TermBlankValue {
private:
    char* str;
    uint len;
}

extern(C)
union TermValue {
private:
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
    @property kind() { return handle.kind; }
    @property bool isURI() { return kind == TermKind.URI; }
    @property bool isLiteral() { return kind == TermKind.literal; }
    @property bool isBlank() { return kind == TermKind.blank; }
    @property URIWithoutFinalize uri() {
        return URIWithoutFinalize.fromHandle(handle.value.uri);
    }
    @property const ref TermLiteralValue literal() {
        return handle.value.literal;
    }
    @property const ref TermBlankValue blank() {
        return handle.value.blank;
    }
}

struct Term {
    mixin WithFinalize!(TermHandle,
                        TermWithoutFinalize,
                        Term,
                        raptor_free_term);
    mixin CompareHandles!(raptor_term_equals, raptor_term_compare);
}

// TODO: Stopped at Language
