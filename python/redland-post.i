/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * redland-post.i - Python SWIG helper routines
 *
 * $Id$
 *
 * Copyright (C) 2000-2004, David Beckett http://purl.org/net/dajobe/
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
    {"unicode_to_bytes",  librdf_python_unicode_to_bytes, METH_VARARGS,
     "Turn a python Unicode string into the UTF-8 bytes."},
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
