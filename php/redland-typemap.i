%typemap(in) librdf_uri* %{
  if(SWIG_ConvertPtr(*$input, (void **) &$1, SWIGTYPE_p_librdf_uri_s, 0) < 0) {
    /* Allow NULL from php for librdf_uri* */
    if ((*$input)->type==IS_NULL)
      $1=NULL;
   else
      SWIG_PHP_Error(E_ERROR, "Type error in argument $argnum of $symname. Expected $1_descriptor");
  }
%}

%typemap(in) librdf_node* %{
  if(SWIG_ConvertPtr(*$input, (void **) &$1, SWIGTYPE_p_librdf_node_s, 0) < 0) {
    /* Allow NULL from php for librdf_node* */
    if ((*$input)->type==IS_NULL)
      $1=NULL;
   else
      SWIG_PHP_Error(E_ERROR, "Type error in argument $argnum of $symname. Expected $1_descriptor");
  }
%}

%typemap(out) librdf_node*, librdf_stream*, librdf_iterator* %{
  if(!$1) {
    ZVAL_NULL(return_value);
  } else {
    SWIG_SetPointerZval(return_value, (void *)$1, $1_descriptor, $owner);
  }
%}

%pragma(php4) phpinfo="
   php_info_print_table_start();
   php_info_print_table_header(2, \"Redland RDF Support\", \"enabled\");
   php_info_print_table_row(2, \"redland librdf version\", librdf_version_string);
   php_info_print_table_row(2, \"redland php bindings version\", REDLAND_BINDINGS_VERSION);
   php_info_print_table_end();
"

%pragma(php5) phpinfo="
   php_info_print_table_start();
   php_info_print_table_header(2, \"Redland RDF Support\", \"enabled\");
   php_info_print_table_row(2, \"redland librdf version\", librdf_version_string);
   php_info_print_table_row(2, \"redland php bindings version\", REDLAND_BINDINGS_VERSION);
   php_info_print_table_end();
"

%mshutdown {
  librdf_php_world_finish();
}

#if PHP_MAJOR_VERSION >= 5
%exception {
  $action
  if(librdf_php_log_message)
    librdf_php_check_exception();
}
#endif
