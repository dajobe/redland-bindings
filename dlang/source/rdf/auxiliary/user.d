module rdf.auxiliary.user;

import core.memory : GC;

/// Internal
class UnmovableObject {
    this() {
        //GC.addRoot(cast(void*)this); // prevents the destructor to work
        GC.setAttr(cast(void*)this, GC.BlkAttr.NO_MOVE);
    }
    ~this() {
       //GC.removeRoot(cast(void*)this);
       GC.clrAttr(cast(void*)this, GC.BlkAttr.NO_MOVE);
   }
}

/// Integration of callbacks with OOP
abstract class UserObject(Record) : UnmovableObject {
    Record record;
    alias record this;
    @property context() { return cast(void*)this; }
}
