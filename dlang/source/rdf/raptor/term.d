module rdf.raptor.term;

import std.string;
import std.typecons;
import rdf.auxiliary.handled_record;
import rdf.auxiliary.nullable_string;
import rdf.raptor.memory;
import rdf.raptor.world;
import rdf.raptor.uri;

private extern extern(C) {
    void raptor_free_term(TermHandle* term);
    TermHandle* raptor_term_copy(TermHandle* term);
    int raptor_term_compare(const TermHandle* t1, const TermHandle* t2);
    int raptor_term_equals(const TermHandle* t1, const TermHandle* t2);
    char* raptor_term_to_string(TermHandle *term);
    TermHandle* raptor_new_term_from_counted_blank(RaptorWorldHandle *world,
                                                   const char *blank,
                                                   size_t length);
    TermHandle* raptor_new_term_from_counted_literal(RaptorWorldHandle* world,
                                                     const char *literal,
                                                     size_t literal_len,
                                                     URIHandle* datatype,
                                                     const char *language,
                                                     char language_len);
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
    /// Return the language tag or empty string if there are none
    @property string language() {
        return _language ? _language[0.._languageLen].idup : "";
    }
}

extern(C)
struct TermBlankValue {
private:
    char* str;
    uint len;
    @property string value() {
        return str[0..len].idup;
    }
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
    @property RaptorWorldWithoutFinalize world() {
        return RaptorWorldWithoutFinalize.fromHandle(handle.world);
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
    string toString() {
        char* str = raptor_term_to_string(handle);
        if(!str) throw new RDFException();
        scope(exit) raptor_free_memory(str);
        return (cast(immutable char*)str).fromStringz; // TODO: Something is probably wrong here.
    }
    // TODO: To_Turtle_String Turtle_Write
}

struct Term {
    mixin WithFinalize!(TermHandle,
                        TermWithoutFinalize,
                        Term,
                        raptor_free_term);
    mixin CompareHandles!(raptor_term_equals, raptor_term_compare);
    static Term fromBlank(RaptorWorldWithoutFinalize world) {
        return Term.fromNonnullHandle(raptor_new_term_from_counted_blank(world.handle, null, 0));
    }
    static Term fromBlank(RaptorWorldWithoutFinalize world, string id) {
        const char* str = id.toStringz;
        TermHandle* handle = raptor_new_term_from_counted_blank(world.handle, str, id.length);
        return fromNonnullHandle(handle);
    }
    static Term fromBlank(RaptorWorldWithoutFinalize world, Nullable!string id) {
        const char* str = id.myToStringz;
        return fromNonnullHandle(raptor_new_term_from_counted_blank(world.handle, str, str ? id.get.length : 0));
    }
    Term From_Literal(RaptorWorldWithoutFinalize world,
                      Nullable!string literal,
                      URIWithoutFinalize datatype,
                      Nullable!string language)
    {
        TermHandle* handle =
            raptor_new_term_from_counted_literal(world.handle,
                                                 literal.myToStringz,
                                                 literal.length,
                                                 datatype.handle,
                                                 language.myToStringz,
                                                 cast(char)language.length);
        return fromNonnullHandle(handle);
    }
}

// TODO: Stopped at From_URI_String
