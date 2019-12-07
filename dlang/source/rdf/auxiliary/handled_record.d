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

// struct Dummy;

mixin template WithoutFinalize(alias Dummy,
                               alias _WithoutFinalize,
                               alias _WithFinalize,
                               alias copier = null)
{
    import std.traits;

    private Dummy* ptr;
    // Use fromHandle() instead
    private this(Dummy* ptr) {
        this.ptr = ptr;
    }
    @property Dummy* handle() const {
        return cast(Dummy*)ptr;
    }
    static _WithoutFinalize fromHandle(const Dummy* ptr) {
        return _WithoutFinalize(cast(Dummy*)ptr);
    }
    static _WithoutFinalize fromNonnullHandle(const Dummy* ptr) {
        if(!ptr) throw new NullRDFException();
        return _WithoutFinalize(cast(Dummy*)ptr);
    }
    @property bool isNull() {
        return ptr == null;
    }
    static if(isCallable!copier) {
        _WithFinalize dup() {
            return _WithFinalize(copier(ptr));
        }
    }
}

mixin template WithFinalize(alias Dummy,
                            alias _WithoutFinalize,
                            alias _WithFinalize,
                            alias destructor,
                            alias constructor = null)
{
    import std.traits;

    private Dummy* ptr;
    @disable this();
    static if (isCallable!constructor) {
        static _WithFinalize create() {
            return _WithFinalize(constructor());
        }
    }
    @disable this(this);
    // Use fromHandle() instead
    private this(Dummy* ptr) {
        this.ptr = ptr;
    }
    ~this() {
        destructor(ptr);
    }
    /*private*/ @property _WithoutFinalize base() { // private does not work in v2.081.2
        return _WithoutFinalize(ptr);
    }
    alias base this;
    @property Dummy* handle() const {
        return cast(Dummy*)ptr;
    }
    static _WithFinalize fromHandle(const Dummy* ptr) {
        return _WithFinalize(cast(Dummy*)ptr);
    }
    static _WithFinalize fromNonnullHandle(const Dummy* ptr) {
        if(!ptr) throw new NullRDFException();
        return _WithFinalize(cast(Dummy*)ptr);
    }
}

mixin template CompareHandles(alias equal, alias compare) {
    import std.traits;

    bool opEquals(const ref typeof(this) s) const {
        static if(isCallable!equal) {
          return equal(handle, s.handle) != 0;
        } else {
          return compare(handle, s.handle) == 0;
        }
    }
    int opCmp(const ref typeof(this) s) const {
      return compare(handle, s.handle);
    }
}
