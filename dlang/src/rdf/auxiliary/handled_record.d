extern(C)
struct Dummy {
    private char dummy = 0; // C99 requires at least one member in struct
}

alias void function (Dummy* ptr) Destructor;

template CObject(Destructor destructor) {
    struct BaseObject {
        private Dummy *obj;

    }
}
