/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * redland-post.i - PHP SWIG helper routines
 *
 */


#if PHP_MAJOR_VERSION >= 5
/* This file seems to have been added in PHP5 */
#include "zend_exceptions.h"
#endif


static librdf_world* librdf_php_get_world(void);

/* When in PHP when being compiled by C */
static librdf_world* librdf_php_world;


static librdf_world*
librdf_php_get_world(void)
{
  return librdf_php_world;
}


static librdf_node*
librdf_php_get_null_node(void)
{
  return NULL;
}

static librdf_uri*
librdf_php_get_null_uri(void)
{
  return NULL;
}

static raptor_locator librdf_php_locator;
static int librdf_php_log_level=0;
static int librdf_php_log_code=0;
static char* librdf_php_log_message=NULL;

static zend_class_entry* redland_exception_ptr=NULL;

static int
librdf_php_last_log_level(void)
{
  return librdf_php_log_level;
}

static int
librdf_php_last_log_code(void)
{
  return librdf_php_log_code;
}

static char*
librdf_php_last_log_message(void)
{
  return librdf_php_log_message;
}

void
librdf_php_free_last_log(void)
{
  librdf_php_log_level=0;
  librdf_php_log_code=0;
  if(librdf_php_log_message)
    free(librdf_php_log_message);
  librdf_php_log_message=NULL;
}

static int
librdf_php_logger_handler(void *user_data, librdf_log_message *log_msg)
{
  raptor_locator* locator = log_msg->locator;

  if(librdf_php_locator.file)
    free((char*)librdf_php_locator.file);
  if(librdf_php_locator.uri)
    raptor_free_uri(librdf_php_locator.uri);
  if(librdf_php_log_message)
    free(librdf_php_log_message);

  if(locator) {
    memcpy(&librdf_php_locator, locator, sizeof(raptor_locator));
    if(locator->file)
      librdf_php_locator.file=strdup(locator->file);
    if(locator->uri)
      librdf_php_locator.uri=raptor_uri_copy(locator->uri);
  } else
    memset(&librdf_php_locator, '\0', sizeof(raptor_locator));

  librdf_php_log_level=log_msg->level;
  librdf_php_log_code=log_msg->code;
  librdf_php_log_message=strdup(log_msg->message);
  
  return 1;
}


void
librdf_php_check_exception(void)
{
  if(!librdf_php_log_message)
    return;
  
#if PHP_MAJOR_VERSION >= 5
  if(librdf_php_log_level >= LIBRDF_LOG_WARN)
    zend_throw_exception(redland_exception_ptr,
                         librdf_php_log_message,
                         librdf_php_log_code TSRMLS_CC);
#endif
  
  librdf_php_log_code=0;
  librdf_php_log_message=NULL;
}


void
librdf_php_world_init(void)
{
  if(!librdf_php_world) {
    zend_class_entry ee_ce;
    zend_class_entry *exception_ce;

    librdf_php_world=librdf_new_world();
    librdf_world_open(librdf_php_world);

#if PHP_MAJOR_VERSION >= 5
    exception_ce = zend_exception_get_default();
    INIT_CLASS_ENTRY(ee_ce, "RedlandException", NULL);
    redland_exception_ptr = zend_register_internal_class_ex(&ee_ce, 
                                                            exception_ce
#if PHP_MAJOR_VERSION < 7
                                                            ,NULL TSRMLS_CC
#endif
                                                            );
#endif

    memset(&librdf_php_locator, '\0', sizeof(raptor_locator));
    librdf_php_log_code=0;
    librdf_php_log_message=NULL;
    
    librdf_world_set_logger(librdf_php_world, NULL, librdf_php_logger_handler);
  }
}

void
librdf_php_world_set_logger(librdf_world *world)
{
  librdf_world_set_logger(world, NULL, librdf_php_logger_handler);
}

void
librdf_php_world_finish(void)
{
  if(librdf_php_world) {
    librdf_free_world(librdf_php_world);
    librdf_php_world=NULL;
  }
}
