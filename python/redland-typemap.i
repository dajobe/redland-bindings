%exception {
  $action
  if(librdf_python_error_message) {
    PyErr_SetString(PyRedland_Error, librdf_python_error_message);
    free(librdf_python_error_message);
    librdf_python_error_message=NULL;
    SWIG_fail;
  }
  if(librdf_python_warning_message) {
    PyErr_Warn(PyRedland_Warning, librdf_python_warning_message);
    free(librdf_python_warning_message);
    librdf_python_warning_message=NULL;
  }
}
