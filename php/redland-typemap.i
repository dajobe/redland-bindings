%typemap(out) librdf_stream*, librdf_iterator* %{
  if(!$1) {
    ZVAL_NULL(return_value);
  } else {
    SWIG_SetPointerZval(return_value, (void *)$1, $1_descriptor, $owner);
  }
%}
