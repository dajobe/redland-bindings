void librdf_python_world_init(librdf_world *world);


/* swig doesn't seem to declare prototypes of get accessors for statics */
static PyObject *_wrap_librdf_copyright_string_get(void);
static PyObject *_wrap_librdf_version_string_get(void);
static PyObject *_wrap_librdf_short_copyright_string_get(void);
static PyObject *_wrap_librdf_version_decimal_get(void);

static PyObject *_wrap_librdf_version_major_get(void);
static PyObject *_wrap_librdf_version_minor_get(void);
static PyObject *_wrap_librdf_version_release_get(void);

SWIGEXPORT(void) SWIG_init(void);

static PyObject *librdf_python_callback = NULL;

static PyObject * librdf_python_set_callback(PyObject *dummy, PyObject *args);

/*
 * set the Python function object callback
 */
static PyObject *
librdf_python_set_callback(dummy, args)
  PyObject *dummy, *args;
{
  PyObject *result = NULL;
  PyObject *temp;
  
  if (PyArg_ParseTuple(args, "O:set_callback", &temp)) {
    if (!PyCallable_Check(temp)) {
      PyErr_SetString(PyExc_TypeError, "parameter must be callable");
      return NULL;
    }
    Py_XINCREF(temp);         /* Add a reference to new callback */
    Py_XDECREF(librdf_python_callback);  /* Dispose of previous callback */
    librdf_python_callback = temp;       /* Remember new callback */
    /* Boilerplate to return "None" */
    Py_INCREF(Py_None);
    result = Py_None;
  }
  return result;
}


/* Declare a table of methods that python can call */
static PyMethodDef librdf_python_methods [] = {
    {"set_callback",  librdf_python_set_callback, METH_VARARGS,
     "Set python message callback."},
    {NULL, NULL, 0, NULL}        /* Sentinel */
};


/*
 * calls a python function defined as:
 *   RDF.message($$)
 * where first argument is an integer, second is a (scalar) string
 * with an integer return value indicating if the message was handled.
 */
static int
librdf_call_python_message(int type, const char *message, va_list arguments)
{
  char empty_buffer[1];
  PyObject *arglist;
  PyObject *result;
  char *buffer;
  int len;
  va_list args_copy;
  int rc=0;

  if(!librdf_python_callback)
    return 0;

  /* ask vsnprintf size of buffer required */
  va_copy(args_copy, arguments);
  len=vsnprintf(empty_buffer, 1, message, args_copy)+1;
  va_end(args_copy);
  buffer=(char*)malloc(len);
  if(!buffer) {
    fprintf(stderr, "librdf_call_python_message: Out of memory\n");
    return 0;
  }
  
  va_copy(args_copy, arguments);
  vsnprintf(buffer, len, message, args_copy);
  va_end(args_copy);

  /* call the callback */
  arglist = Py_BuildValue("(is)", type, buffer);
  if(!arglist) {
    fprintf(stderr, "librdf_call_python_message: Out of memory\n");
    free(buffer);
    return 0;
  }
  result = PyEval_CallObject(librdf_python_callback, arglist);
  Py_DECREF(arglist);
  if(result) {
    if(PyInt_Check(result))
      rc=(int)PyInt_AS_LONG(result);
    
    Py_DECREF(result);
  }

  free(buffer);

  rc=1;
  
  return rc;
}

static int
librdf_python_error_handler(void *user_data, 
                            const char *message, va_list arguments)
{
  return librdf_call_python_message(0, message, arguments);
}


static int
librdf_python_warning_handler(void *user_data,
                              const char *message, va_list arguments)
{
  return librdf_call_python_message(1, message, arguments);
}

void
librdf_python_world_init(librdf_world *world)
{
  (void) Py_InitModule("Redland_python", librdf_python_methods);
  librdf_world_set_error(world, NULL, librdf_python_error_handler);
  librdf_world_set_warning(world,  NULL, librdf_python_warning_handler);
}
