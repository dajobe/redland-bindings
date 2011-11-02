static librdf_world* librdf_php_get_world(void);
static void librdf_php_world_set_logger(librdf_world*);
static void librdf_php_world_finish(void);

static librdf_node* librdf_php_get_null_node(void);
static librdf_uri* librdf_php_get_null_uri(void);

static int librdf_php_last_log_level(void);
static int librdf_php_last_log_code(void);
static char* librdf_php_last_log_message(void);
