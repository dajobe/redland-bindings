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

%mshutdown {
  if(librdf_php_world) {
    librdf_free_world(librdf_php_world);
    librdf_php_world=NULL;
  }
}
