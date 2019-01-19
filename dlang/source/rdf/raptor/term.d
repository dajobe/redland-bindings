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
    TermHandle* raptor_new_term_from_counted_uri_string(RaptorWorldHandle* world,
                                                        const char *uri_string,
                                                        size_t length);
    TermHandle* raptor_new_term_from_uri(RaptorWorldHandle* world, URIHandle* uri);
    TermHandle* raptor_new_term_from_counted_string(RaptorWorldHandle* world,
                                                    const char *string,
                                                    size_t length);
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
    // TODO:
   //   Term_2: Term_Type := From_URI_String(World, "http://example.org/abc");
   //   Term_3: Term_Type := From_URI(World, From_String(World, "http://example.org/cvb"));
   //   Term_4: Term_Type := From_String(World, """ZZZ"""); -- Turtle string
   //begin
   //   Assert(Value(Get_Literal(Term_1)) = "QWE", "Term_1 value");
   //   Assert(To_String(Get_URI(Term_2)) = "http://example.org/abc", "Term_2 URI");
   //   Assert(To_String(Get_URI(Term_3)) = "http://example.org/cvb", "Term_3 URI");
   //   Assert(Value(Get_Literal(Term_4)) = "ZZZ", "Term_4 value");
   //   Assert(To_String(Datatype(Get_Literal(Term_1))) = "http://example.org", "Term_1 datatype"); -- infinite loop! why?
   //   Assert(Term_1 /= Term_2, "Non-equal terms");
   //   Assert(Get_Kind(Term_1) = Literal, "Term_1 is literal");
   //   Assert(Get_Kind(Term_2) = URI, "Term_2 is URI");
   //   Assert(Get_Kind(Term_3) = URI, "Term_3 is URI");
   //   Assert(Get_Kind(Term_4) = Literal, "Term_4 is literal");
   //
}

