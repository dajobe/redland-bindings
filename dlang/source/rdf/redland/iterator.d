module rdf.redland.iterator;

import rdf.auxiliary.handled_record;

// Usually you should use Base_With_Finalization types, such as defined in RDF.Redland.Iterator_Iterator

// TODO: Necessarily test that it works as expected

struct IteratorHandle;

private extern extern(C) {
    void librdf_free_iterator(IteratorHandle* iterator);
    int librdf_iterator_end(IteratorHandle* iterator);
    int librdf_iterator_next(IteratorHandle* iterator);
}

struct IteratorWithoutFinalize {
    mixin WithoutFinalize!(IteratorHandle,
                           IteratorWithoutFinalize,
                           Iterator);
    @property bool empty() {
        return librdf_iterator_end(handle) != 0;
    }
    @property IteratorWithoutFinalize front() { return this; }
    void popFront() {
        cast(void)librdf_iterator_next(handle);
    }
}

struct Iterator {
    mixin WithFinalize!(IteratorHandle,
                        IteratorWithoutFinalize,
                        Iterator,
                        librdf_free_iterator);
}

// TODO: Stopped at Get_Object_Internal

