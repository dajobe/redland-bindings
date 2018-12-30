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

alias extern(C) void function (Dummy* ptr) Destructor;
alias extern(C) Dummy* function () Constructor;
alias extern(C) Dummy* function (Dummy* ptr) Copier;

template CObject(Destructor destructor,
                 Constructor constructor = null,
                 Copier copier = null) {
    struct WithoutFinalization {
        private Dummy* ptr;
        static if (constructor) {
            WithoutFinalization create() {
                return WithoutFinalization(constructor());
            }
        }
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
            return WithoutFinalization(ptr);
        }
        static from_nonnull_handle(Dummy* ptr) {
            if(!ptr) throw NullRDFException;
            return WithoutFinalization(ptr);
        }
        @propery bool is_null() {
            return ptr;
        }
        static if(copier) {
            WithFinalization dup() {
                return WithFinalization(ptr);
            }
        }
    }

    struct WithFinalization {
        private Dummy* ptr;
        static if (constructor) {
            WithoutFinalization create() {
                return WithFinalization(constructor());
            }
        }
        @disabled this(this);
        // Use from_handle() instead
        private this(Dummy* ptr) {
            this.ptr = ptr;
        }
        ~this() {
            destructor(ptr);
        }
        private @property void base() {
            return WithoutFinalization(ptr);
        }
        alias base this;
        static from_handle(Dummy* ptr) {
            return WithFinalization(ptr);
        }
        static from_nonnull_handle(Dummy* ptr) {
            if(!ptr) throw NullRDFException;
            return WithFinalization(ptr);
        }
    }
}
