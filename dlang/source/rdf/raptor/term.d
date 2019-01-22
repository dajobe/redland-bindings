module rdf.raptor.term;

import std.string;
import std.typecons;
import rdf.auxiliary.handled_record;
import rdf.auxiliary.nullable_string;
import rdf.raptor.memory;
import rdf.raptor.world;
import rdf.raptor.uri;
import rdf.raptor.namespace_stack;
import rdf.raptor.iostream;

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
    TermHandle* raptor_new_term_from_counted_uri_string(RaptorWorldHandle* world,
                                                        const char *uri_string,
                                                        size_t length);
    TermHandle* raptor_new_term_from_uri(RaptorWorldHandle* world, URIHandle* uri);
    TermHandle* raptor_new_term_from_counted_string(RaptorWorldHandle* world,
                                                    const char *string,
                                                    size_t length);
    char* raptor_term_to_turtle_string(TermHandle *term,
                                       NamespaceStackHandle* nstack,
                                       URIHandle* base_uri);
    int raptor_term_turtle_write(IOStreamHandle *iostr,
                                 TermHandle *term,
                                 NamespaceStackHandle* nstack,
                                 URIHandle* base_uri);
}

enum TermKind { unknown = 0,
                uri     = 1,
                literal = 2,
                // unused type 3
                blank   = 4 }

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

struct TermBlankValue {
private:
    char* str;
    uint len;
    @property string value() {
        return str[0..len].idup;
    }
}

union TermValue {
private:
    URIHandle* uri;
    TermLiteralValue literal;
    TermBlankValue blank;
}

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
    @property bool isURI() { return kind == TermKind.uri; }
    @property bool isLiteral() { return kind == TermKind.literal; }
    @property bool isBlank() { return kind == TermKind.blank; }
    @property URIWithoutFinalize uri() {
        return URIWithoutFinalize.fromHandle(handle.value.uri);
    }
    @property ref const(TermLiteralValue) literal() {
        return handle.value.literal;
    }
    @property ref const(TermBlankValue) blank() {
        return handle.value.blank;
    }
    string toString() {
        char* str = raptor_term_to_string(handle);
        if(!str) throw new RDFException();
        scope(exit) raptor_free_memory(str);
        return str.fromStringz.idup;
    }
    string toTurtleString(NamespaceStackWithoutFinalize stack, URIWithoutFinalize baseURI) {
        char* str = raptor_term_to_turtle_string(handle, stack.handle, baseURI.handle);
        // Better use raptor_term_to_turtle_counted_string() instead (here and in Ada)
        if(!str) throw new RDFException();
        scope(exit) raptor_free_memory(str);
        return str.fromStringz.idup;
    }
    void turtleWrite(IOStreamWithoutFinalize stream,
                     NamespaceStackWithoutFinalize stack,
                     URIWithoutFinalize baseURI)
    {
        if(raptor_term_turtle_write(stream.handle, handle, stack.handle,baseURI.handle) != 0)
            throw new RDFException();
    }
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
    static Term fromLiteral(RaptorWorldWithoutFinalize world,
                            Nullable!string literal,
                            URIWithoutFinalize datatype,
                            Nullable!string language)
    {
        TermHandle* handle =
            raptor_new_term_from_counted_literal(world.handle,
                                                 literal.myToStringz,
                                                 literal.myLength,
                                                 datatype.handle,
                                                 language.myToStringz,
                                                 cast(char)language.myLength);
        return fromNonnullHandle(handle);
    }
    static Term fromURIString(RaptorWorldWithoutFinalize world, string uri) {
        TermHandle* handle =
            raptor_new_term_from_counted_uri_string(world.handle, uri.ptr, uri.length);
        return fromNonnullHandle(handle);
    }
    static Term fromURI(RaptorWorldWithoutFinalize world, URIWithoutFinalize uri) {
        return fromNonnullHandle(raptor_new_term_from_uri(world.handle, uri.handle));
    }
    static Term fromString(RaptorWorldWithoutFinalize world, string uriString) {
        return fromNonnullHandle(raptor_new_term_from_counted_string(world.handle, uriString.ptr, uriString.length));
    }
}

unittest {
    RaptorWorld world = RaptorWorld.createAndOpen();
    Term term1 = Term.fromLiteral(world,
                                  Nullable!string("QWE"),
                                  URI.fromString(world, "http://example.org"), // datatype
                                  Nullable!string()); // langage
    Term term2 = Term.fromURIString(world, "http://example.org/abc");
    Term term3 = Term.fromURI(world, URI.fromString(world, "http://example.org/cvb"));
    // .dup below is necessary due to http://bugs.librdf.org/mantis/view.php?id=648
    Term term4 = Term.fromString(world, "\"ZZZ\"".dup); // Turtle string
    // TODO:
//    assert(term1.literal.value == "QWE", "Term_1 value");
//    assert(term2.uri.toString == "http://example.org/abc", "Term_2 URI");
//    assert(term3.uri.toString == "http://example.org/cvb", "Term_3 URI");
//    assert(term3.literal.value == "ZZZ", "Term_4 value");
//    assert(term1.literal.datatype.toString == "http://example.org", "Term_1 datatype"); // infinite loop! why?
//    assert(term1 != term2, "Non-equal terms");
//    with(TermKind) {
//        assert(term1.kind == literal, "Term_1 is literal");
//        assert(term2.kind == uri, "Term_2 is URI");
//        assert(term3.kind == uri, "Term_3 is URI");
//        assert(term4.kind == literal, "Term_4 is literal");
//    }
}

