#include <Rdefines.h>
#include <Rinternals.h>
#include <Rmath.h>

SEXP isnull(SEXP pointer) {
    void *ptr = R_ExternalPtrAddr(pointer);
    SEXP rvalue = PROTECT(NEW_LOGICAL(1));
    if (ptr==NULL) {
        LOGICAL_DATA(rvalue)[0] = (Rboolean)TRUE;
    } else {
        LOGICAL_DATA(rvalue)[0] = (Rboolean)FALSE;
    }
    UNPROTECT(1);
    return(rvalue);
}
