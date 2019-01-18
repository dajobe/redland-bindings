module rdf.auxiliary.user;

import core.memory : GC;

class UnmovableObject {
    this() {
        GC.setAttr(cast(void*)this, GC.BlkAttr.NO_MOVE);
    }
    // Called AFTER object finalization:
//    ~this() {
//        GC.clrAttr(cast(void*)this, GC.BlkAttr.NO_MOVE);
//    }
}

/// Integration of callbacks with OOP
// FIXME: Remove template argument
abstract class UserObject(Record) : UnmovableObject {
    //alias record this; // prevents compilation as of v2.083.1
    @property void* context() { return cast(void*)this; }
}
