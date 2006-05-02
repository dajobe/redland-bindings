#ifdef SWIGRUBY
/* optional input strings - can be NULL, need special conversions */
%typemap(ruby,in) const char *inStrOrNull {
  $1 = ($input == Qnil) ? NULL : STR2CSTR($input);
}
/* returning char* or NULL, need special conversions */
%typemap(ruby,out) char *{
 $result = ($1 == NULL) ? Qnil : rb_str_new2($1);
}
#endif
