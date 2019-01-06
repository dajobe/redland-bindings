module rdf.raptor.www;

import rdf.auxiliary.handled_record;

private extern extern(C) {
    void raptor_free_www(WWWHandle* www);
}

/// I deliberately expose that it is a pointer type,
/// to simplify libcurl and libxml interaction
struct Connection;

struct WWWHandle;

struct WWWWithoutFinalize {
    mixin WithoutFinalize!(WWWHandle,
                           WWWWithoutFinalize,
                           WWW);
}

struct WWW {
    mixin WithFinalize!(WWWHandle,
                        WWWWithoutFinalize,
                        WWW,
                        raptor_free_www);
}

// TODO: Stopped at Initialize_All_Callbacks
