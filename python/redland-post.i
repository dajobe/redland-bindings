/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * redland-post.i - Python SWIG helper routines
 *
 * $Id$
 *
 * Copyright (C) 2000-2005, David Beckett http://purl.org/net/dajobe/
 * Institute for Learning and Research Technology http://www.ilrt.bristol.ac.uk/
 * University of Bristol, UK http://www.bristol.ac.uk/
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
 * complete terms and further detail along with the license texts for
 * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
 * 
 * 
 */

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
static PyObject * librdf_python_reset_callback(PyObject *dummy, PyObject *args);

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
  PyObject *result = NULL;
  PyObject *temp;
  
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
    size_t len=PyUnicode_GET_DATA_SIZE(unicod);
    int i, j;
    
    output=(char*)malloc(len+1); /* too long but saves double-scanning */
    if(!output)
      goto failure;
    
    len/=sizeof(Py_UNICODE);
    
    j=0;
    for(i=0; i < len; i++) {
      int size=raptor_unicode_char_to_utf8((unsigned long)input[i], &output[j]);
      if(size <= 0)
        goto failure;
      j+= size;
    } 
    output[j]='\0';

    result=PyString_FromStringAndSize(output, j+1);
  }

 failure:
  if(output)
    free(output);
  return result;
}


/* Declare a table of methods that python can call */
static PyMethodDef librdf_python_methods [] = {
  {"set_callback",  librdf_python_set_callback, METH_VARARGS, 
   "Set python message callback."},
  {"reset_callback",  librdf_python_reset_callback, METH_VARARGS, 
   "Set python message callback."},
  {"unicode_to_bytes",  librdf_python_unicode_to_bytes, METH_VARARGS,
   "Turn a python Unicode string into the UTF-8 bytes."},
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
librdf_python_logger_handler(void *user_data, librdf_log_message *log)
{
  raptor_locator* locator = log->locator;
  int line= -1;
  int column= -1;
  int byte= -1;
  const char *uri=NULL;
  const char *file=NULL;
  
  if(locator) {
    line=raptor_locator_line(locator);
    column=raptor_locator_column(locator);
    byte=raptor_locator_byte(locator);
    file=raptor_locator_file(locator);
    uri=raptor_locator_uri(locator);
  }
  
  if(librdf_python_callback)
    return librdf_call_python_message(log->code, log->level, log->facility,
                                      log->message,
                                      line, column, byte, file, uri);
  

  if(log->level < LIBRDF_LOG_WARN)
    return 1;
  
  return librdf_python_message_handler((log->level < LIBRDF_LOG_ERROR),
                                       log->message);
}


void
librdf_python_world_init(librdf_world *world)
{
  PyObject *module;
  PyObject *dict;
  PyObject *tuple;

  module = Py_InitModule("Redland_python", librdf_python_methods);
  dict = PyModule_GetDict(module);

  tuple = Py_BuildValue ("(iii)", librdf_version_major, librdf_version_minor,
                         librdf_version_release);
  PyDict_SetItemString(dict, "version", tuple);
  Py_DECREF(tuple);

  PyRedland_Warning = PyErr_NewException("RDF.RedlandWarning", 
                                         PyExc_Warning, NULL);
  PyDict_SetItemString(dict, "Warning", PyRedland_Warning);

  PyRedland_Error = PyErr_NewException("RDF.RedlandError",
                                       PyExc_RuntimeError, NULL);
  PyDict_SetItemString(dict, "Error", PyRedland_Error);

  librdf_world_set_logger(world, NULL, librdf_python_logger_handler);
}
