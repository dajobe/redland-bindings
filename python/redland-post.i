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
    {"unicode_to_bytes",  librdf_python_unicode_to_bytes, METH_VARARGS,
     "Turn a python Unicode string into the UTF-8 bytes."},
    {NULL, NULL, 0, NULL}        /* Sentinel */
};


/*
 * stores a redland error message for later
 */
static int
librdf_python_message_handler(int type, const char *message, va_list arguments)
{
  char empty_buffer[1];
  int len;
  va_list args_copy;
  char **buffer;

  if(type)
    buffer=&librdf_python_warning_message;
  else
    buffer=&librdf_python_error_message;

  if(*buffer) {
    /* There is already a pending warning or error, return not handled */
    return 0;
    /* alternative: discard the older one with free(*buffer); */
  }

  /* ask vsnprintf size of buffer required */
  va_copy(args_copy, arguments);
  len=vsnprintf(empty_buffer, 1, message, args_copy)+1;
  va_end(args_copy);

  *buffer=(char*)malloc(len);
  if(!*buffer) {
    fprintf(stderr, "librdf_python_message_handler: Out of memory\n");
    return 0;
  }
  
  va_copy(args_copy, arguments);
  vsnprintf(*buffer, len, message, args_copy);
  va_end(args_copy);

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
librdf_python_error_handler(void *user_data, 
                            const char *message, va_list arguments)
{
  return librdf_python_message_handler(0, message, arguments);
}


static int
librdf_python_warning_handler(void *user_data,
                              const char *message, va_list arguments)
{
  return librdf_python_message_handler(1, message, arguments);
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

  librdf_world_set_error(world, NULL, librdf_python_error_handler);
  librdf_world_set_warning(world, NULL, librdf_python_warning_handler);
}
