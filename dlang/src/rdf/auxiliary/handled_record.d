class RDFException: Exception { }
class NonNullRDFException: RDFException { }

extern(C)
struct Dummy {
    private char dummy = 0; // C99 requires at least one member in struct
}

alias extern(C) void function (Dummy* ptr) Destructor;
alias extern(C) Dummy* function () Constructor;

template CObject(Destructor destructor, Constructor constructor = null) {
    struct WithoutFinalization {
        private Dummy* ptr;
        static if (constructor) {
            this() {
                ptr = constructor();
            }
        }
        this(Dummy* ptr) {
            this.ptr = ptr;
        }
        ~this() {
            destructor(ptr);
        }
        @property Dummy* handle() {
            return ptr;
        }
        static from_nonnull_handle(Dummy* ptr) {
            if(!ptr) throw NonNullRDFException;
            return WithoutFinalization(ptr);
        }
    }

    struct WithFinalization {
        private Dummy* ptr;
        static if (constructor) {
            this() {
                ptr = constructor();
            }
        }
        this(Dummy* ptr) {
            this.ptr = ptr;
        }
        ~this() {
            destructor(ptr);
        }
        private @property void base() {
            return WithoutFinalization(ptr);
        }
        alias base this;
    }
}
