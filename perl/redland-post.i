void librdf_perl_world_init(librdf_world *world);
void librdf_perl_world_finish(void);


/*
 * calls a perl subroutine defined as:
 *   RDF::Redland::World::message($$)
 * where first argument is an integer, second is a (scalar) string
 */
static int
librdf_call_perl_message(int type, const char *message, va_list arguments)
{
  char empty_buffer[1];
  dSP;
  char *buffer;
  int len;
  va_list args_copy;
  int rc=0;
  
  SAVETMPS;

  /* ask vsnprintf size of buffer required */
  va_copy(args_copy, arguments);
  len=vsnprintf(empty_buffer, 1, message, args_copy)+1;
  va_end(args_copy);
  buffer=(char*)malloc(len);
  if(!buffer)
    fprintf(stderr, "librdf_call_perl_message: Out of memory\n");
  else {
    int count;
    
    va_copy(args_copy, arguments);
    vsnprintf(buffer, len, message, args_copy);
    va_end(args_copy);

    PUSHMARK(SP) ;
    XPUSHs(sv_2mortal(newSViv(type)));
    XPUSHs(sv_2mortal(newSVpv(buffer, 0)));
    PUTBACK;
  
    count=call_pv("RDF::Redland::World::message", G_SCALAR);

    SPAGAIN;
    if(count == 1)
      rc=POPi;
    PUTBACK;
    
    free(buffer);
  }
  
  FREETMPS;
  
  return rc;
}

static int
librdf_perl_error_handler(void *user_data, 
                          const char *message, va_list arguments)
{
  return librdf_call_perl_message(0, message, arguments);
}


static int
librdf_perl_warning_handler(void *user_data,
                            const char *message, va_list arguments)
{
  return librdf_call_perl_message(1, message, arguments);
}

static librdf_world* librdf_perl_world=NULL;

void
librdf_perl_world_init(librdf_world *world)
{
  librdf_world_set_error(world, NULL, librdf_perl_error_handler);
  librdf_world_set_warning(world,  NULL, librdf_perl_warning_handler);

  librdf_perl_world=world;
}

void
librdf_perl_world_finish(void)
{
  librdf_free_world(librdf_perl_world);
}
