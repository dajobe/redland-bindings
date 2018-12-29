extern(C)
struct Dummy {
    private char dummy = 0; // C99 requires at least one member in struct
}

alias extern(C) void function (Dummy* ptr) Destructor;
alias extern(C) Dummy* function () Constructor;

template CObject(Destructor destructor, Constructor constructor = null) {
    struct BaseObject {
        private Dummy *ptr;
        static if (constructor) {
            this() {
                ptr = constructor();
            }
        }
        this(Dummy *ptr) {
            this.ptr = ptr;
        }
        ~this() {
            destructor(ptr);
        }
    }

    struct WithFinalization {
        private Dummy *ptr;
        static if (constructor) {
            this() {
                ptr = constructor();
            }
        }
        this(Dummy *ptr) {
            this.ptr = ptr;
        }
        ~this() {
            destructor(ptr);
        }
        private @property void base() {
            return BaseObject(ptr);
        }
        alias base this;
    }
}
