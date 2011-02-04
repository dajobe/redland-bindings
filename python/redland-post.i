/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * redland-post.i - Python SWIG helper routines
 *
 * Copyright (C) 2000-2006, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2005, University of Bristol, UK http://www.bristol.ac.uk/
 * 
 * This package is Free Software and part of Redland http://librdf.org/
 * 
 * It is licensed under the following three licenses as alternatives:
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 * 
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 * 
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * full license terms.
 * 
 * 
 */

void librdf_python_world_init(librdf_world *world);


void SWIG_init(void);

static PyObject *librdf_python_callback = NULL;

static PyObject * librdf_python_set_callback(PyObject *dummy, PyObject *args);
static PyObject * librdf_python_reset_callback(PyObject *dummy, PyObject *args);
static PyObject * librdf_python_set_parser_uri_filter(PyObject *dummy, PyObject *args);


/*
 * set the Python function object callback
 */
static PyObject *
librdf_python_set_callback(PyObject *dummy, PyObject *args)
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


/*
 * set the Python function object callback
 */
static PyObject *
librdf_python_reset_callback(PyObject *dummy, PyObject *args)
{
  if(librdf_python_callback) {
    Py_XDECREF(librdf_python_callback);  /* Dispose of previous callback */
    librdf_python_callback = NULL;
  }

  /* Boilerplate to return "None" */
  Py_INCREF(Py_None);
  return Py_None;
}


/*
 * calls a python function defined as:
 *   RDF.message(level, message, line, file, uri)
 * where first argument is the log leve, second is a (scalar) string
 * message, line number, file (or None), uri (or None)
 * with an integer return value indicating if the message was handled.
 */
static int
librdf_call_python_message(int code, int level, int facility,
                           const char *message,
                           int line, int column, int byte,
                           const char *file, const char *uri)
{
  PyObject *arglist;
  PyObject *result;
  int rc=0;

  if(!librdf_python_callback)
     return 0;
 
  /* call the callback */
  arglist = Py_BuildValue("(iiisiiiss)", code, level, facility, message, line, column, byte, file, uri);
  if(!arglist) {
    fprintf(stderr, "librdf_call_python_message: Out of memory\n");
    return 0;
  }
  result = PyEval_CallObject(librdf_python_callback, arglist);
  Py_DECREF(arglist);
  if(result) {
    if(PyInt_Check(result))
      rc=(int)PyInt_AS_LONG(result);
    
    Py_DECREF(result);
   }

  rc=1;
  
  return rc;
}


static char* librdf_python_error_message = NULL;
static char* librdf_python_warning_message = NULL;

static PyObject *PyRedland_Warning;
static PyObject *PyRedland_Error;

/*
 * helper function for turning a python Unicode string into the UTF-8
 * set of bytes suitable for calling Redland string functions
 */
static PyObject *
librdf_python_unicode_to_bytes(PyObject *dummy, PyObject *args) 
{
  PyObject *result = NULL;
  PyObject *unicod;
  char *output = NULL;

  if (PyArg_ParseTuple(args, "U:unicode_to_bytes", &unicod)) {
    Py_UNICODE* input=(Py_UNICODE*)PyUnicode_AS_DATA(unicod);
    /* turn size of Python Unicode string - UCS2 encoded into size
     * needed for UTF-8 encoded C-string 
     */
    size_t input_len = PyUnicode_GET_SIZE(unicod);
    size_t output_len;
    int i, j;
    
    /* Turn Unicode string length into max possible UTF-8 length (1-3
     * bytes/char)
     */
    output_len = (3 * input_len);

    output = (char*)malloc(output_len + 1);
    if(!output) {
      PyErr_SetString(PyExc_MemoryError, "Out of memory");
      goto failure;
    }

    j = 0;
    for(i=0; i < input_len; i++) {
      int size = raptor_unicode_utf8_string_put_char((unsigned long)input[i], 
                                                     (unsigned char*)&output[j],
                                                     output_len - j);
      if(size <= 0) {
        PyErr_SetString(PyExc_ValueError, "Invalid input Unicode");
        goto failure;
      }

      j += size;
    } 
    output[j] = '\0';

    result = PyString_FromStringAndSize(output, j + 1);
  }

 failure:
  if(output)
    free(output);
  return result;
}


static int
librdf_call_python_uri_filter(void* user_data, librdf_uri* uri) 
{
  PyObject *arglist;
  PyObject *result;
  int rc=0;
  PyObject *callback=(PyObject*)user_data;
  
  /* call the callback */
  arglist = Py_BuildValue("(s)", librdf_uri_as_string(uri));
  if(!arglist) {
    fprintf(stderr, "librdf_call_python_uri_filter: Out of memory\n");
    return 0;
  }
  result = PyEval_CallObject(callback, arglist);
  Py_DECREF(arglist);
  if(result) {
    if(PyInt_Check(result))
      rc=(int)PyInt_AS_LONG(result);
    
    Py_DECREF(result);
  } else
    rc=1;

  return rc;
}


/*
 * set the parser URI filter callback
 */
static PyObject *
librdf_python_set_parser_uri_filter(PyObject *dummy, PyObject *args)
{
  PyObject *result = NULL;
  PyObject *temp;
  PyObject * obj0 = 0 ;
  void *argp1 = 0 ;
  int res1 = 0 ;
  librdf_parser* parser=NULL;
  librdf_uri_filter_func current_filter;
  void* current_filter_user_data;
  
  if (PyArg_ParseTuple(args, "OO:set_parser_uri_filter", &obj0, &temp)) {
    /* first argument: SWIG wrapped librdf_parser* */
    res1 = SWIG_ConvertPtr(obj0, &argp1,SWIGTYPE_p_librdf_parser_s, 0 |  0 );
    if (!SWIG_IsOK(res1)) {
      SWIG_exception_fail(SWIG_ArgError(res1), "in 'librdf_python_set_parser_uri_filter', argument 1 of type 'librdf_parser *'"); 
    }
    parser = (librdf_parser *)(argp1);

    /* second argument: PyObject* callback function */
    if (!PyCallable_Check(temp)) {
      PyErr_SetString(PyExc_TypeError, "parameter must be callable");
      return NULL;
    }

    /* Add a reference to new callback */
    Py_XINCREF(temp);

    /* FIXME: The above object reference should be remembered and
     * Py_XDECREFed when the parser is destroyed
     */

    /* Check for any existing callback to discard */
    current_filter=librdf_parser_get_uri_filter(parser,
                                                &current_filter_user_data);
    if(current_filter) {
      Py_XDECREF((PyObject*)current_filter_user_data);
    }

    /* Set new callback */
    librdf_parser_set_uri_filter(parser, librdf_call_python_uri_filter, temp);

    /* Boilerplate to return "None" */
    Py_INCREF(Py_None);
    result = Py_None;
  }
  return result;
fail:
  return NULL;
}


/* Declare a table of methods that python can call */
static PyMethodDef librdf_python_methods [] = {
  {"set_callback",  librdf_python_set_callback, METH_VARARGS, 
   "Set python message callback."},
  {"reset_callback",  librdf_python_reset_callback, METH_VARARGS, 
   "Set python message callback."},
  {"unicode_to_bytes",  librdf_python_unicode_to_bytes, METH_VARARGS,
   "Turn a python Unicode string into the UTF-8 bytes."},
  {"set_parser_uri_filter",  librdf_python_set_parser_uri_filter, METH_VARARGS, 
   "Set a parser's URI filter function"},
  {NULL, NULL, 0, NULL}        /* Sentinel */
};


/*
 * stores a redland error message for later
 */
static int
librdf_python_message_handler(int is_warning, const char *message)
{
  int len;
  char **buffer;

  if(is_warning)
    buffer=&librdf_python_warning_message;
  else
    buffer=&librdf_python_error_message;

  if(*buffer) {
    /* There is already a pending warning or error, return not handled */
    return 0;
    /* alternative: discard the older one with free(*buffer); */
  }

  len=strlen(message);
  *buffer=(char*)malloc(len+1);
  if(!*buffer) {
    fprintf(stderr, "librdf_python_message_handler: Out of memory\n");
    return 0;
  }
  
  strncpy(*buffer, message, len+1);

  /*
   * Emit warnings right away
   * Note: this makes the %exception code for warning never run
   */
  if(*buffer == librdf_python_warning_message) {
    PyErr_Warn(PyRedland_Warning, librdf_python_warning_message);
    free(librdf_python_warning_message);
    librdf_python_warning_message=NULL;
  }
  
  return 1;
}


static int
librdf_python_logger_handler(void *user_data, librdf_log_message *log_msg)
{
  raptor_locator* locator = log_msg->locator;
  int line= -1;
  int column= -1;
  int byte= -1;
  const char *uri=NULL;
  const char *file=NULL;
  
  if(locator) {
    line = raptor_locator_line(locator);
    column = raptor_locator_column(locator);
    byte = raptor_locator_byte(locator);
    file = raptor_locator_file(locator);
    uri = raptor_locator_uri(locator);
  }
  
  if(librdf_python_callback)
    return librdf_call_python_message(log_msg->code, log_msg->level, 
                                      log_msg->facility, log_msg->message,
                                      line, column, byte, file, uri);
  

  if(log_msg->level < LIBRDF_LOG_WARN)
    return 1;
  
  return librdf_python_message_handler((log_msg->level < LIBRDF_LOG_ERROR),
                                       log_msg->message);
}


void
librdf_python_world_init(librdf_world *world)
{
  PyObject *module;
  PyObject *dict;
  PyObject *tuple;
  PyObject* rdf_module;
  const char *module_name="RDF";
  
  module = Py_InitModule("Redland_python", librdf_python_methods);
  dict = PyModule_GetDict(module); /* borrowed reference */

  tuple = Py_BuildValue ("(iii)", librdf_version_major, librdf_version_minor,
                         librdf_version_release);
  PyDict_SetItemString(dict, "version", tuple);
  Py_DECREF(tuple);

  rdf_module=PyImport_ImportModule((char*)module_name);
  if(rdf_module != NULL) {
    PyObject* rdf_module_dict;
    
    rdf_module_dict = PyModule_GetDict(rdf_module); /* borrowed reference */

    PyRedland_Warning = PyDict_GetItemString(rdf_module_dict,
                                             "RedlandWarning");
    
    PyRedland_Error = PyDict_GetItemString(rdf_module_dict,
                                           "RedlandError");

    Py_DECREF(rdf_module);
  } else {
    PyErr_Print();
    fprintf(stderr, "Failed to import module \"%s\"\n", module_name);
  }
  
  librdf_world_set_logger(world, NULL, librdf_python_logger_handler);
}
