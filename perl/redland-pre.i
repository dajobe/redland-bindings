/* for perl, these are passed in by MakeMaker derived makefile */
#undef PACKAGE
#undef VERSION

/* Delete one of many names polluted by Perl in embed.h via perl.h */
#ifdef list
#undef list
#endif
