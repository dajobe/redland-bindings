module rdf.redland.iterator;

import rdf.auxiliary.handled_record;

// Usually you should use Base_With_Finalization types, such as defined in RDF.Redland.Iterator_Iterator

// TODO: Necessarily test that it works as expected

struct IteratorHandle;

private extern extern(C) {
    void librdf_free_iterator(IteratorHandle* iterator);
}

struct IteratorWithoutFinalize {
    mixin WithoutFinalize!(IteratorHandle,
                           IteratorWithoutFinalize,
                           Iterator);
}

struct Iterator {
    mixin WithFinalize!(IteratorHandle,
                        IteratorWithoutFinalize,
                        Iterator,
                        librdf_free_iterator);
}

