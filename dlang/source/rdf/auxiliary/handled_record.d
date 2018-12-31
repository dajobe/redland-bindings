module rdf.auxiliary.handled_record;

import std.traits;

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

mixin template WithoutFinalization(alias _WithoutFinalization,
                                   alias _WithFinalization,
                                   alias destructor,
                                   alias constructor = null,
                                   alias copier = null)
{
    private Dummy* ptr;
    // Use from_handle() instead
    private this(Dummy* ptr) {
        this.ptr = ptr;
    }
    ~this() {
        destructor(ptr);
    }
    @property Dummy* handle() {
        return ptr;
    }
    static from_handle(Dummy* ptr) {
        return _WithoutFinalization(ptr);
    }
    static from_nonnull_handle(Dummy* ptr) {
        if(!ptr) throw new NullRDFException();
        return _WithoutFinalization(ptr);
    }
    @property bool is_null() {
        return ptr == null;
    }
    static if(copier) {
        _WithFinalization dup() {
            return _WithFinalization(copier(ptr));
        }
    }
}

mixin template WithFinalization(alias _WithoutFinalization,
                                alias _WithFinalization,
                                alias destructor,
                                alias constructor = null,
                                alias copier = null) {
    private Dummy* ptr;
    static if (isCallable!constructor) {
        WithoutFinalization create() {
            return WithFinalization(constructor());
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
    private @property _WithoutFinalization base() {
        return _WithoutFinalization(ptr);
    }
    alias base this;
    static from_handle(Dummy* ptr) {
        return _WithFinalization(ptr);
    }
    static from_nonnull_handle(Dummy* ptr) {
        if(!ptr) throw new NullRDFException();
        return _WithFinalization(ptr);
    }
}
