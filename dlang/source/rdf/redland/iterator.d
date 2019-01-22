module rdf.redland.iterator;

import rdf.auxiliary.handled_record;
import rdf.redland.world;

// Usually you should use Base_With_Finalization types, such as defined in RDF.Redland.Iterator_Iterator

struct IteratorHandle;

private extern extern(C) {
    void librdf_free_iterator(IteratorHandle* iterator);
    int librdf_iterator_end(IteratorHandle* iterator);
    int librdf_iterator_next(IteratorHandle* iterator);
    void* librdf_iterator_get_object(IteratorHandle* iterator);
    void* librdf_iterator_get_context(IteratorHandle* iterator);
    void* librdf_iterator_get_key(IteratorHandle* iterator);
    void* librdf_iterator_get_value(IteratorHandle* iterator);
    IteratorHandle* librdf_new_empty_iterator(RedlandWorldHandle* world);
}

struct IteratorWithoutFinalize {
    mixin WithoutFinalize!(IteratorHandle,
                           IteratorWithoutFinalize,
                           Iterator);
    @property bool empty() const {
        return librdf_iterator_end(handle) != 0;
    }
    @property const(IteratorWithoutFinalize) front() const { return this; }
    void popFront() {
        cast(void)librdf_iterator_next(handle);
    }
    @property void* _object() const {
        return librdf_iterator_get_object(handle);
    }
    @property void* _context() const {
        return librdf_iterator_get_context(handle);
    }
    @property void* _key() const {
        return librdf_iterator_get_key(handle);
    }
    @property void* _value() const {
        return librdf_iterator_get_value(handle);
    }
    // librdf_iterator_add_map() not implemented
}

struct Iterator {
    mixin WithFinalize!(IteratorHandle,
                        IteratorWithoutFinalize,
                        Iterator,
                        librdf_free_iterator);
    static Iterator emptyIterator(RedlandWorldWithoutFinalize world) {
        return Iterator.fromNonnullHandle(librdf_new_empty_iterator(world.handle));
    }
}

