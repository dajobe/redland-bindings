module rdf.auxiliary.handled_record;

class RDFException: Exception {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
    this(string file = __FILE__, size_t line = __LINE__) {
        this("RDF error", file, line);
    }
}

class NullRDFException: RDFException {
    this(string msg, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
    }
    this(string file = __FILE__, size_t line = __LINE__) {
        this("librdf null pointer exception", file, line);
    }
}

extern(C)
struct Dummy {
    private char dummy = 0; // C99 requires at least one member in struct
}

mixin template WithoutFinalize(alias _WithoutFinalize,
                               alias _WithFinalize,
                               alias copier = null)
{
    //import std.traits;

    private Dummy* ptr;
    // Use from_handle() instead
    private this(Dummy* ptr) {
        this.ptr = ptr;
    }
    @property Dummy* handle() {
        return ptr;
    }
    static from_handle(Dummy* ptr) {
        return _WithoutFinalize(ptr);
    }
    static from_nonnull_handle(Dummy* ptr) {
        if(!ptr) throw new NullRDFException();
        return _WithoutFinalize(ptr);
    }
    @property bool is_null() {
        return ptr == null;
    }
    static if(copier) {
        _WithFinalize dup() {
            return _WithFinalize(copier(ptr));
        }
    }
}

mixin template WithFinalize(alias _WithoutFinalize,
                            alias _WithFinalize,
                            alias destructor,
                            alias constructor = null)
{
    import std.traits;

    private Dummy* ptr;
    static if (isCallable!constructor) {
        static _WithFinalize create() {
            return _WithFinalize(constructor());
        }
    }
    @disable this(this);
    // Use from_handle() instead
    private this(Dummy* ptr) {
        this.ptr = ptr;
    }
    ~this() {
        destructor(ptr);
    }
    private @property _WithoutFinalize base() {
        return _WithoutFinalize(ptr);
    }
    alias base this;
    static from_handle(Dummy* ptr) {
        return _WithFinalize(ptr);
    }
    static from_nonnull_handle(Dummy* ptr) {
        if(!ptr) throw new NullRDFException();
        return _WithFinalize(ptr);
    }
}
