module rdf.raptor.term;

import std.string;
import rdf.auxiliary.handled_record;
import rdf.raptor.memory;

struct TermHandle;

private extern extern(C) {
    void raptor_free_term(TermHandle* term);
    TermHandle* raptor_term_copy(TermHandle* term);
    int raptor_term_compare(const TermHandle* t1, const TermHandle* t2);
    int raptor_term_equals(const TermHandle* t1, const TermHandle* t2);
}

struct TermWithoutFinalize {
    mixin WithoutFinalize!(TermHandle,
                           TermWithoutFinalize,
                           Term,
                           raptor_term_copy);
    mixin CompareHandles!(raptor_term_equals, raptor_term_compare);
}

struct Term {
    mixin WithFinalize!(TermHandle,
                        TermWithoutFinalize,
                        Term,
                        raptor_free_term);
    mixin CompareHandles!(raptor_term_equals, raptor_term_compare);
}

// TODO: Stopped at Term_Kind
