librdf_world* librdf_php_get_world(void);
void librdf_php_world_finish(void);


/* When in PHP when being compiled by C */
static librdf_world* librdf_php_world;

static librdf_world*
librdf_php_get_world(void)
{
  return librdf_php_world;
}

static void
librdf_php_world_finish(void)
{
  librdf_free_world(librdf_php_world);
}
