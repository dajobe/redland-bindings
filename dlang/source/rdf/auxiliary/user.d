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
abstract class UserObject : UnmovableObject {
    //alias record this; // prevents compilation as of v2.083.1
    final @property void* context() const { return cast(void*)this; }
}
